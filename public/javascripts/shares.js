// date formatter taken from http://jacwright.com/projects/javascript/date_format under MIT license
Date.prototype.format=function(format){var returnStr='';var replace=Date.replaceChars;for(var i=0;i<format.length;i++){var curChar=format.charAt(i);if(i-1>=0&&format.charAt(i-1)=="\\"){returnStr+=curChar;}else if(replace[curChar]){returnStr+=replace[curChar].call(this);}else if(curChar!="\\"){returnStr+=curChar;}}return returnStr;};Date.replaceChars={shortMonths:['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],longMonths:['January','February','March','April','May','June','July','August','September','October','November','December'],shortDays:['Sun','Mon','Tue','Wed','Thu','Fri','Sat'],longDays:['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'],d:function(){return(this.getDate()<10?'0':'')+this.getDate();},D:function(){return Date.replaceChars.shortDays[this.getDay()];},j:function(){return this.getDate();},l:function(){return Date.replaceChars.longDays[this.getDay()];},N:function(){return this.getDay()+1;},S:function(){return(this.getDate()%10==1&&this.getDate()!=11?'st':(this.getDate()%10==2&&this.getDate()!=12?'nd':(this.getDate()%10==3&&this.getDate()!=13?'rd':'th')));},w:function(){return this.getDay();},z:function(){var d=new Date(this.getFullYear(),0,1);return Math.ceil((this-d)/86400000);},W:function(){var d=new Date(this.getFullYear(),0,1);return Math.ceil((((this-d)/86400000)+d.getDay()+1)/7);},F:function(){return Date.replaceChars.longMonths[this.getMonth()];},m:function(){return(this.getMonth()<9?'0':'')+(this.getMonth()+1);},M:function(){return Date.replaceChars.shortMonths[this.getMonth()];},n:function(){return this.getMonth()+1;},t:function(){var d=new Date();return new Date(d.getFullYear(),d.getMonth(),0).getDate()},L:function(){var year=this.getFullYear();return(year%400==0||(year%100!=0&&year%4==0));},o:function(){var d=new Date(this.valueOf());d.setDate(d.getDate()-((this.getDay()+6)%7)+3);return d.getFullYear();},Y:function(){return this.getFullYear();},y:function(){return(''+this.getFullYear()).substr(2);},a:function(){return this.getHours()<12?'am':'pm';},A:function(){return this.getHours()<12?'AM':'PM';},B:function(){return Math.floor((((this.getUTCHours()+1)%24)+this.getUTCMinutes()/60+this.getUTCSeconds()/3600)*1000/24);},g:function(){return this.getHours()%12||12;},G:function(){return this.getHours();},h:function(){return((this.getHours()%12||12)<10?'0':'')+(this.getHours()%12||12);},H:function(){return(this.getHours()<10?'0':'')+this.getHours();},i:function(){return(this.getMinutes()<10?'0':'')+this.getMinutes();},s:function(){return(this.getSeconds()<10?'0':'')+this.getSeconds();},u:function(){var m=this.getMilliseconds();return(m<10?'00':(m<100?'0':''))+m;},e:function(){return"Not Yet Supported";},I:function(){return"Not Yet Supported";},O:function(){return(-this.getTimezoneOffset()<0?'-':'+')+(Math.abs(this.getTimezoneOffset()/60)<10?'0':'')+(Math.abs(this.getTimezoneOffset()/60))+'00';},P:function(){return(-this.getTimezoneOffset()<0?'-':'+')+(Math.abs(this.getTimezoneOffset()/60)<10?'0':'')+(Math.abs(this.getTimezoneOffset()/60))+':00';},T:function(){var m=this.getMonth();this.setMonth(0);var result=this.toTimeString().replace(/^.+ \(?([^\)]+)\)?$/,'$1');this.setMonth(m);return result;},Z:function(){return-this.getTimezoneOffset()*60;},c:function(){return this.format("Y-m-d\\TH:i:sP");},r:function(){return this.toString();},U:function(){return this.getTime()/1000;}};

//if a person image fails to load then load the person0.jpg
function handlePersonImageError(source){
    source.src = "/public/images/person0.jpg";
    source.onerror = "";
    return true;
}


function saveToEverNote(updateKey) {
    console.log("Just dropped share " + updateKey);
}


shareList = '<ul>';
numShares = 0;

function mainParse(data) {
/*    console.log("json I got back: " + JSON.stringify(data)); */
    $.each(data.values, function(valueidx, value){
        var updateKey, firstName, lastName, pictureUrl, title, submittedUrl, thumbnailUrl, comment;
        updateKey = value.updateKey;
        person = value.updateContent.person;
        if (person) {
          firstName = person.firstName;
          lastName = person.lastName;
          pictureUrl = person.pictureUrl;

          if (person.currentShare) {
              content = person.currentShare.content;
              if (content) {
                  title = content.title;
                  submittedUrl = content.submittedUrl;
                  thumbnailUrl = content.thumbnailUrl;
                  comment = person.currentShare.comment;
                  shareList += '<li><span id="' + updateKey + '">';
                  shareList += '<img class="user-pic" height="40" width="40" src="' + pictureUrl + '"/>';
                  shareList += firstName + ' ' + lastName + ' &#0187 ';
                  shareList += '<a href="' + submittedUrl + '" target="_blank">';
                  shareList += '<img class="thumbnail" height="40" src="' + thumbnailUrl  + '"/>';
                  shareList += title;
                  shareList += '</a>\n';
                  if (comment) {
                    shareList += '<p class="comment">' + comment + '</p>';
                  }
                  shareList += '</li>';
                  numShares += 1;
              }
          }
          else {
            console.log("no share for " + valueidx);
          }
        }
    });
    shareList += '</ul>';

    $('#shares').html(shareList);
    console.log("There are " + numShares + " shares");

    var eat = ['yum!', 'gulp', 'burp!', 'nom'];
    var yum = document.createElement('p');
    var msie = /*@cc_on!@*/0;
    yum.style.opacity = 1;

    var links = document.querySelectorAll('li > span'), el = null;
    console.log("there are " + links.length + " links");
    for (var i = 0; i < links.length; i++) {
      el = links[i];

      el.setAttribute('draggable', 'true');

      addEvent(el, 'dragstart', function (e) {
        e.dataTransfer.effectAllowed = 'copy'; // only dropEffect='copy' will be dropable
        e.dataTransfer.setData('Text', this.id); // required otherwise doesn't work
      });
    }

    var bin = document.querySelector('#bin');

    addEvent(bin, 'dragover', function (e) {
      if (e.preventDefault) e.preventDefault(); // allows us to drop
      this.className = 'over';
      e.dataTransfer.dropEffect = 'copy';
      return false;
    });

    // to get IE to work
    addEvent(bin, 'dragenter', function (e) {
      this.className = 'over';
      return false;
    });

    addEvent(bin, 'dragleave', function () {
      this.className = '';
    });

    addEvent(bin, 'drop', function (e) {
      if (e.stopPropagation) e.stopPropagation(); // stops the browser from redirecting...why???

      var shareID = e.dataTransfer.getData('Text');
      var el = document.getElementById(shareID);
      el.parentNode.removeChild(el);

      //make the ajax call here to save the share
      saveToEverNote(shareID)

      // stupid nom text + fade effect
      bin.className = '';
      yum.innerHTML = 'Saved to EverNote';

      var y = yum.cloneNode(true);
      bin.appendChild(y);

      setTimeout(function () {
        var t = setInterval(function () {
          if (y.style.opacity <= 0) {
            if (msie) { // don't bother with the animation
              y.style.display = 'none';
            }
            clearInterval(t);
          } else {
            y.style.opacity -= 0.2;
          }
        }, 100);

        y.style.opacity = 0;
      }, 1000);

      return false;
    });


}

$(function(){
	$.getJSON('/application/shares', mainParse)
});


