const app = Elm.Main.init()


app.ports.controlVideo.subscribe(message => {
    var vid = document.getElementById("earthVideo"); 
    if (message) {
      vid.play()
    } else
      vid.pause()
  });
  
  app.ports.setPhoneInputCursor.subscribe(pos => {
    var phoneInput = document.getElementById("phoneInput");
    phoneInput.focus()
    phoneInput.setSelectionRange(pos, pos);
  });
  
  var winX = null;
  var winY = null;
  app.ports.disableScrolling.subscribe(msg => {
    if (msg) {
      winX = window.scrollX;
      winY = window.scrollY;
    } else {
      winX = null;
      winY = null;
    }
  });
  window.addEventListener('scroll', function () {
    if (winX !== null && winY !== null) {
        window.scrollTo(winX, winY);
    }
  });
  
  window.addEventListener("scroll", function(event) {
    app.ports.recvScroll.send(document.documentElement.scrollTop);
  });