package controllers

import play._
import play.mvc._
import play.mvc.results._


import org.scribe.builder._
import org.scribe.builder.api._
import org.scribe.model._
import org.scribe.oauth._
import scala.xml._
import java.util.{List=>JList,ArrayList=>JArrayList,HashMap=>JHashMap,Map=>JMap}
import java.lang.{Integer=>JInteger}


import java.security.MessageDigest
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.THttpClient

import com.evernote.edam.userstore._
import com.evernote.edam.error._
import com.evernote.edam.userstore.Constants
import com.evernote.edam.notestore._
import scala.collection.JavaConversions._
import com.evernote.edam.`type`.{Note, Notebook}
;


/**
 * This is the main controller for In2EverNote done for HackDay 7/15/2011.
 *
 * The main controller for the application.
 * There are four actions: index, connections, profile, and nus
 *
 * index -- any user can see it without authentication
 * connections -- shows a page of connections after authenticating
 * nus -- shows network updates
 * profile --shows your profile
 */
object Application extends Controller {

  val inDebugMode = false
//  println("InDebugMode = " + inDebugMode)

  // Values stored in the session
  val KEY_REQUEST_TOKEN = "requestToken"
  val KEY_REQUEST_TOKEN_SECRET = "requestTokenSecret"
  val KEY_ACCESS_TOKEN = "accessToken"
  val KEY_ACCESS_TOKEN_SECRET = "accessTokenSecret"
  val KEY_EVERNOTE_TOKEN = "evernoteToken"
  val KEY_EVERNOTE_SHARD_ID = "evernoteShardID"
  val KEY_EVERNOTE_NOTEBOOK_GUID = "evernoteNBGUID"


  //supposed to be thread safe
  val oauthService = new ServiceBuilder()
    .provider(classOf[LinkedInApi])
    .apiKey(ApiKeys.apiKey)
    .apiSecret(ApiKeys.secretKey)
    .callback(ApiKeys.url)
    .build();


  //val evernoteHost = "sandbox.evernote.com"
  val evernoteHost = "www.evernote.com"
  //println("Using evernoteHost: " + evernoteHost)
  val userStoreUrl = "https://" + evernoteHost + "/edam/user"
  val noteStoreUrlBase = "https://" + evernoteHost + "/edam/note/"

  val notebookName = "In2Evernote"

  // Change the User Agent to a string that describes your application, using
  // the form company name/app name and version. Using a unique user agent string
  // allows us to identify applications in our logs and provide you with better support.
  val userAgent = "Evernote/EDAMDemo (Java) " +
                  Constants.EDAM_VERSION_MAJOR + "." +
                  Constants.EDAM_VERSION_MINOR;

  /**
   * Shows the main intro page (no need to authenticate here)
   */
  def index = Template()

  def undefined =  {
    println("request for undefined")
    NoContent
  }

  // just removes all 4 keys from the session
  private def cleanSession() = {
    //println("Cleaning session!")
    session.remove(KEY_ACCESS_TOKEN)
    session.remove(KEY_ACCESS_TOKEN_SECRET)
    session.remove(KEY_REQUEST_TOKEN)
    session.remove(KEY_REQUEST_TOKEN_SECRET)
  }

  //the request token string and secret make up the request token and are stored in the session
  private def rebuildRequestToken() = {
    val token = session.get(KEY_REQUEST_TOKEN)
    val secret = session.get(KEY_REQUEST_TOKEN_SECRET)
//    println("Rebuilding with request token        " + token)
//    println("Rebuilding with request token secret " + secret)
    new Token(token, secret)
  }

  //save the access token in the session since we'll resuse it
  private def saveAccessToken(accessToken: Token): Unit = {
    session.put(KEY_ACCESS_TOKEN, accessToken.getToken())
    session.put(KEY_ACCESS_TOKEN_SECRET, accessToken.getSecret())
  }

  /**
   * Handles the 3 authentication cases:
   * 1) just got redirected from LinkedIn
   * 2) alrady logged in (has access token in the session)
   * 3) have nothing and need to redirect to LinkedIn for auth
   *
   * The method is defined as taking oauth_token and oauth_verifier.
   * The "oauth_token" is the request token value, LinkedIn just calls the callback URL
   * and passes that param back as well as the oauth_verifier code which is what we really need,
   * so we can ignore the oauth_token param.
   */
  private def authenticate(oauth_token: String, oauth_verifier: String): (Token, Boolean) = {
    val accessTokenToken = session.get(KEY_ACCESS_TOKEN)
    val accessTokenSecret = session.get(KEY_ACCESS_TOKEN_SECRET)
    val needsRedirect = true

    if (accessTokenToken != null && accessTokenSecret != null) {
      //have the access token already so get its parts out of the session and reconstruct
//      println("Already logged in")
//      println("session access token         :" + accessTokenToken)
//      println("session access token secret  :" + accessTokenSecret)
      val accessToken = new Token(accessTokenToken, accessTokenSecret)
      (accessToken, !needsRedirect)
    }
    else if (oauth_verifier != null) {
      //got redirected from LinkedIn and the oauth_verifier is passed as a parameter
      //println("Redirected from LinkedIn with oauth_verifier " + oauth_verifier)
      val verifier = new Verifier(oauth_verifier)
      val accessToken = oauthService.getAccessToken(rebuildRequestToken(), verifier);
      cleanSession();
      saveAccessToken(accessToken);
      (accessToken, !needsRedirect)
    }
    else {
      //println("Fresh Start")
      val requestToken = oauthService.getRequestToken()
      //println("got request token: " + requestToken.toString())
      (requestToken, needsRedirect)
    }
  }

  /**
   * Tries the main logic to display a template but if fails, then logs the error and redirects to index.
   */
  private def doAndRedirectToIndexOnError(oauth_token: String, oauth_verifier: String, mainAction: (Token => Result)): Result = {
    try {
      val (token, needsRedirect) = authenticate(oauth_token, oauth_verifier)
      if (needsRedirect) {
        doRedirect(token)
      } else {
        mainAction(token)
      }
    } catch {
      case e: Exception =>
        println("Failed due to " + e.getMessage)
        cleanSession()
        Action(index)
    }
  }

  /**
   * Redirects to the authorization URL from LinkedIn
   */
  private def doRedirect(requestToken: Token) = {
    //now redirect to the authorization url from LinkedIn. the callback will bring us back
    //to this method but we'll have additional request parameters for oauth_token and oauth_verifier
    val url = oauthService.getAuthorizationUrl(requestToken)

    //in playframework we can only store Strings in the cookie based "session", so
    //just stash the 2 components of the requestToken
    cleanSession();
    session.put(KEY_REQUEST_TOKEN, requestToken.getToken())
    session.put(KEY_REQUEST_TOKEN_SECRET, requestToken.getSecret())
    //println("Redirecting to " + url + "\n\n")
    Redirect(url)
  }


  /**
   * Makes a REST API call and returns the result
   */
  private def makeApiCall(accessToken: Token, restUrl: String) = {
    //create and sign a request for the resource
    val orequest: OAuthRequest = new OAuthRequest(Verb.GET, restUrl);
    oauthService.signRequest(accessToken, orequest);

    //actually send the request and get the xml body back
    val oresponse: Response = orequest.send();
    val body = oresponse.getBody();
//    println("Got back" + body)
    body
  }

  // just gets my profile info and displays the XML data
  def profile(oauth_token: String, oauth_verifier: String) = {
    def doProfile(token: Token): Result = {
      //println("Getting ready to make a profile call")
      val restUrl = "http://api.linkedin.com/v1/people/~:(id,first-name,last-name,picture-url)"
      val apiResponse = makeApiCall(token, restUrl)
      Template(apiResponse)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doProfile)
  }

  // just gets my network updates info and displays the XML data
  def nus(oauth_token: String, oauth_verifier: String) = {
    def doNus(token: Token): Result = {
      //println("Getting ready to make a nus call")
      val restUrl = "http://api.linkedin.com/v1/people/~/network/updates?scope=self"
      val apiResponse = makeApiCall(token, restUrl)
      Template(apiResponse)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doNus)
  }

  /**
   * The main action for the site, just showing pictures of your connections
   */
  def connections(oauth_token: String, oauth_verifier: String) = {
    def doConns(token: Token): Result = {
      //println("Getting ready to make a connections call")
      val restUrl = "http://api.linkedin.com/v1/people/~/connections:(id,first-name,last-name,picture-url)"
      val apiResponse = makeApiCall(token, restUrl)
      val people = parseConnectionXml(apiResponse)
      //Template('people -> people) //Play 1.2 syntax
      Template(people)
    }

    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doConns)
  }

  /**
   * Gets share data and outputs a simple JSON response with just the data we need
   * to handle in the UI.
   */
  def shares(oauth_token: String, oauth_verifier: String, offset: String) = {
    val count = 10
    val start = if (offset == null) 0 else (offset.toInt * count)

    def doShares(token: Token): Result = {
      //println("Getting ready to make an NUS SHAR call with start " + start)
      val restUrl = "http://api.linkedin.com/v1/people/~/network/updates?type=SHAR&format=json&count=" + count + "&start=" + start
      //println("\t" + restUrl)
      val apiResponse = makeApiCall(token, restUrl)
      Json(apiResponse)
    }

    if (inDebugMode) {
      println("Reading saved data from shares2.json")
      val jsondata = scala.io.Source.fromFile("In2EverNote/test/shares2.json").getLines().mkString("\n")
      println("read\n" + jsondata)
      Json(jsondata)
    }
    else doAndRedirectToIndexOnError(oauth_token, oauth_verifier, doShares)
  }


  /**
   * Simple class to hold person info
   */
  case class Person(val firstName: String, val lastName: String, val picture: String)

  /**
   * Parse the XML response from the API and return a list of Person
   */
  private def parseConnectionXml(apiResponse: String) : Seq[Person]= {
    val xml = XML.loadString(apiResponse)
    val people = xml \\ "person"
    people.map(p => {
      val firstName = (p \ "first-name").text
      val lastName = (p \ "last-name").text
      val picture = (p \ "picture-url").text
      Person(firstName, lastName, picture)
    })
  }

  case class Share(val firstName: String, val lastName: String, val headline :String, val submittedUrl :String,
                   val title:String, val comment: String)

  private def parseNusXml(apiResponse: String) : Seq[Share]= {
//    val xml = XML.loadString(apiResponse)
//    val people = xml \\ "person"
//    people.map(p => {
//      val firstName = (p \ "first-name").text
//      val lastName = (p \ "last-name").text
//      val picture = (p \ "picture-url").text
//      Share(firstName, lastName, headline)
//    })
    List()
  }


  // page to go to after linkedin login to show the evernote login info
  def evernoteindex(oauth_token: String, oauth_verifier: String, failMsg: String) = {
    def showEverNoteLogin(token: Token): Result = {
      //println("Getting ready to make a profile call")
      val restUrl = "http://api.linkedin.com/v1/people/~:(id,first-name,last-name,picture-url)"
      val apiResponse = makeApiCall(token, restUrl)
      val people = parseConnectionXml(apiResponse)
      val myProfile = people.head
      val today = new java.util.Date
      println("Cool, " + myProfile.firstName + " " + myProfile.lastName + " logged in at " + today.toString)
      if (failMsg != null) { println("failMsg=" + failMsg) }
      val failureMsg = failMsg
      Template(myProfile,failureMsg)
    }
    doAndRedirectToIndexOnError(oauth_token, oauth_verifier, showEverNoteLogin)
  }

  /**
   * This does the connection to evernote, and creates the notebook if necessary.
   * This then takes us to the page that shows the drag and drop info. (It gets the
   * shares through an AJAX call)
   */
  def evernote(username :String, password: String) = {
    if (inDebugMode) {
      val numNotebooks = 300
      Template(numNotebooks)
    }
    else {
      var userStore :  UserStore.Client = null
      var authToken : String = null
      var newNoteGuid : String = null
      var failMsg : String = null

      val userStoreTrans = new THttpClient(userStoreUrl);
      userStoreTrans.setCustomHeader("User-Agent", userAgent);
      val userStoreProt = new TBinaryProtocol(userStoreTrans);
      userStore = new UserStore.Client(userStoreProt, userStoreProt);

      // Check that we can talk to the server
      val versionOk = userStore.checkVersion("Evernote EDAMDemo (Java)",
          com.evernote.edam.userstore.Constants.EDAM_VERSION_MAJOR,
          com.evernote.edam.userstore.Constants.EDAM_VERSION_MINOR);
      if (!versionOk) {
        throw new RuntimeException("Incomatible EDAM client protocol version");
      }

      // Authenticate using username & password
      var authResult : com.evernote.edam.userstore.AuthenticationResult = null;
      try {
        authResult = userStore.authenticate(username, password, ApiKeys.everNoteApiKey, ApiKeys.everNoteSecretKey);
      } catch {
        case ex : EDAMUserException =>
        // Note that the error handling here is far more detailed than you would
        // provide to a real user. It is intended to give you an idea of why the
        // sample application isn't able to authenticate to our servers.

        // Any time that you contact us about a problem with an Evernote API,
        // please provide us with the exception parameter and errorcode.
        val parameter = ex.getParameter();
        val errorCode = ex.getErrorCode();

        System.err.println("Authentication failed (parameter: " + parameter + " errorCode: " + errorCode + ")");

        if (errorCode == EDAMErrorCode.INVALID_AUTH) {
          if (parameter.equals("consumerKey")) {
            System.err.println("Your consumer key was not accepted by " + evernoteHost);
            System.err.println("This sample client application requires a client API key. If you requested a web service API key, you must authenticate using OAuth as shown in sample/java/oauth");
            System.err.println("If you do not have an API Key from Evernote, you can request one from http://www.evernote.com/about/developer/api");
            failMsg = "Application error"
          } else if (parameter.equals("username")) {
            failMsg = "You must authenticate using a username and password from " + evernoteHost;
            if (evernoteHost.equals("www.evernote.com") == false) {
              failMsg += "Note that your production Evernote account will not work on " + evernoteHost + ",";
              failMsg += "you must register for a separate test account at https://" + evernoteHost + "/Registration.action";
            }
          } else if (parameter.equals("password")) {
            failMsg = "The password that you entered is incorrect";
          }
        }

        //throw new RuntimeException("failed to auth to evernote");
        failMsg = "Failed to login to evernote!"
      }

      if (failMsg == null) {
        // The result of a succesful authentication is an opaque authentication token
        // that you will use in all subsequent API calls. If you are developing a
        // web application that authenticates using OAuth, the OAuth access token
        // that you receive would be used as the authToken in subsquent calls.
        authToken = authResult.getAuthenticationToken();
        session.put(KEY_EVERNOTE_TOKEN,authToken)

        // The Evernote NoteStore allows you to accessa user's notes.
        // In order to access the NoteStore for a given user, you need to know the
        // logical "shard" that their notes are stored on. The shard ID is included                   A
        // in the URL used to access the NoteStore.
        val user = authResult.getUser();
        val shardId = user.getShardId();
        session.put(KEY_EVERNOTE_SHARD_ID, shardId)
        //System.out.println("Successfully authenticated as " + user.getUsername());

        val noteStore = getNoteStore();
        val linkedInNotebook = findLinkedInNotebook(noteStore);
        val linkedInNotebookGuid = linkedInNotebook.getGuid
        session.put(KEY_EVERNOTE_NOTEBOOK_GUID, linkedInNotebookGuid)

        val notebooks = noteStore.listNotebooks(authToken);
        val numNotebooks = notebooks.size
        //println("There are " + numNotebooks + "notebooks. " + notebookName + " guid is " + linkedInNotebookGuid)
        Template(numNotebooks, linkedInNotebookGuid)
      }
      else {
        println(failMsg)
        Action(evernoteindex(null,null,failMsg))
      }
    }
  }

  /**
   * Pulls needed values from the session and creates a NoteStore client for EverNote
   */
  def getNoteStore() : NoteStore.Client = {
    val authToken = session.get(KEY_EVERNOTE_TOKEN);
    val shardId = session.get(KEY_EVERNOTE_SHARD_ID);
    val noteStoreUrl = noteStoreUrlBase + shardId;
    val noteStoreTrans = new THttpClient(noteStoreUrl);
    noteStoreTrans.setCustomHeader("User-Agent", userAgent);
    val noteStoreProt = new TBinaryProtocol(noteStoreTrans);
    new NoteStore.Client(noteStoreProt, noteStoreProt);
  }

  def findSpanValue(noteHtml:String, clazz : String) : String = {
    val startMarker = "<span class=\"" + clazz + "\">"
    val idx = noteHtml.indexOf(startMarker)
    val idx2 = idx + startMarker.size
    val idx3 = noteHtml.indexOf("</span", idx2)
    noteHtml.substring(idx2, idx3)
  }

  case class SaveNoteResult(guid: String, didSave: Boolean)

  /**
   * Saves the share to EverNote (We look it back up to avoid letting someone "save" a fake share.
   */
  def savenote(noteHtml : String) = {
//    val restUrl = "http://api.linkedin.com/v1/people/~/network/updates?type=SHAR&format=json"
//    val apiResponse = makeApiCall(token, restUrl)
//    val shares = parseNusXml(apiResponse)

    //we want to store the note with the person's name (and ID to make it unique)
    //val memberid = findSpanValue(noteHtml, "hidden user-memberid")
    val membername = findSpanValue(noteHtml, "user-name")

    //now go through the shares and find the updateKey
    val enml = makeENML(noteHtml)
    //println("Created enml:\n" + enml);

    //now try to save the note to our In2Evernote Notebook
    val evernoteToken = session.get(KEY_EVERNOTE_TOKEN)
    val noteStore  = getNoteStore()
    val linkedInNotebook = findLinkedInNotebook(noteStore);

    val note = new Note()
    val noteTitle = membername
    note.setTitle(noteTitle)
    note.setContent(enml)
    note.setNotebookGuid(session.get(KEY_EVERNOTE_NOTEBOOK_GUID))

    try {
      val newNote = noteStore.createNote(evernoteToken, note);
      //println("Created new note with guid " + newNote.getGuid)
      val result = SaveNoteResult(newNote.getGuid,true)
      Json(result)
    }
    catch {
      case e : Exception =>
        println("Failed to save note\n" + enml);
        e.printStackTrace();
        Json(SaveNoteResult("",false))
    }
  }

  /**
   * Since EverNote ML is similar to XHTML, we can just re-use the display of the note!
   */
  def makeENML(noteHtml : String) : String = {
//    println("Got noteHtml:\n" + noteHtml)

    //strip out the classes and ids and onerror
    val c0 = noteHtml.replaceAll( """ class=".*?"""", " ")
    val c1 = c0.replaceAll( """ id=".*?"""", " ")
    val c2 = c1.replaceAll( """ onerror=".*?"""", " ")
    val c3 = c2.replaceAll("<br>","<br></br>")
    val sb = new StringBuilder(c3)

    //have to add in </img>
    val idx1 = sb.indexOf("<img")
    if (idx1 > -1) {
      val idx2 = sb.indexOf(">", idx1)
      sb.insert(idx2 + 1, "</img>")

      val idx3 = sb.indexOf("<img", idx2)
      if (idx3 > -1) {
        val idx4 = sb.indexOf(">", idx3)
        sb.insert(idx4 + 1, "</img>")
      }
    }

    val content = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\">" +
        "<en-note>" +
        sb.toString +
        "</en-note>";
    content
  }

  def findLinkedInNotebook(noteStore : NoteStore.Client) : Notebook = {
    val authToken = session.get(KEY_EVERNOTE_TOKEN)
    val notebooks = noteStore.listNotebooks(authToken);
    var ourbooks = notebooks.filter(n => {notebookName == n.getName})
    val rv = if (ourbooks.isEmpty) {
      //println("User does not have notebook: " + notebookName)
      var ourbook = new Notebook
      ourbook.setName(notebookName)
      ourbook = noteStore.createNotebook(authToken, ourbook)
      if (ourbook.getGuid == null) {
        //println("created notebook but got null guid, querying back...")
        ourbook = findLinkedInNotebook(noteStore)
        //println("found notebook " + ourbook.getName + " with guid " + ourbook.getGuid)
      }
//      else {
//        //println("Created notebook " + ourbook.getName + " with guid " + ourbook.getGuid)
//      }
      ourbook
    }
    else {
      val ourbook = ourbooks.head
      //println("Found notebook: " + ourbook.getName)
      ourbook
    }
    session.put(KEY_EVERNOTE_NOTEBOOK_GUID, rv.getGuid)
    rv
  }

  /**
   * Retrieve and display a list of the user's notes.
   */
  def listNotes() {
    val authToken = session.get(KEY_EVERNOTE_TOKEN)
    // List all of the notes in the user's account
    println("Listing all notes:");
    val noteStore = getNoteStore()

    // First, get a list of all notebooks
    val notebooks = noteStore.listNotebooks(authToken);

    for (notebook <- notebooks) {
      println("Notebook: " + notebook.getName());

      // Next, search for the first 100 notes in this notebook, ordering by creation date
      val filter = new NoteFilter();
      filter.setNotebookGuid(notebook.getGuid());
//      filter.setOrder(NoteSortOrder.CREATED.getValue());
      filter.setAscending(true);

      val noteList : NoteList = noteStore.findNotes(authToken, filter, 0, 100);
      val notes = noteList.getNotes();
      for (note <- notes) {
        println(" * " + note.getTitle());
      }
    }
    println();
  }
}
