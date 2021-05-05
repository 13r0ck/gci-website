  const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem('storage'))
  })


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
  
//storage
app.ports.save.subscribe(storage => {
  localStorage.setItem('storage', JSON.stringify(storage))
  app.ports.load.send(storage)
})

// Handle smoothly scrolling to links
const scrollToHash = () => {
  const BREAKPOINT_XL = 1920
  const NAVBAR_HEIGHT_PX = window.innerWidth > BREAKPOINT_XL ? 127 : 102
  const element = window.location.hash && document.querySelector(window.location.hash)
  if (element) {
    // element.scrollIntoView({ behavior: 'smooth' })
    window.scroll({ behavior: 'smooth', top: window.pageYOffset + element.getBoundingClientRect().top - NAVBAR_HEIGHT_PX })
  } else {
    window.scroll({ behavior: 'auto', top: 0 })
  }
}

app.ports.onUrlChange.subscribe(_ => setTimeout(scrollToHash, 0))