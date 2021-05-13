const app = Elm.Main.init({
  flags: { width: window.innerWidth, height: window.innerHeight, storage: JSON.parse(localStorage.getItem("storage")) },
});

app.ports.controlVideo.subscribe((message) => {
  var vid = document.getElementById("earthVideo");
  if (message) {
      vid.muted = true;
      vid.play();
  } else vid.pause();
});

app.ports.setPhoneInputCursor.subscribe((pos) => {
  var phoneInput = document.getElementById("phoneInput");
  phoneInput.focus();
  phoneInput.setSelectionRange(pos, pos);
});

var winX = null;
var winY = null;
app.ports.disableScrolling.subscribe((msg) => {
  if (msg) {
      winX = window.scrollX;
      winY = window.scrollY;
  } else {
      winX = null;
      winY = null;
  }
});
window.addEventListener("scroll", function () {
  if (winX !== null && winY !== null) {
      window.scrollTo(winX, winY);
  }
});

enableRecvScroll = true;
var ticking = 0;
window.addEventListener("scroll", function (event) {
  if (ticking > 10) {
    window.requestAnimationFrame(function() {
      app.ports.recvScroll.send(document.documentElement.scrollTop);
      ticking = 0;
    });
  }
    ticking += 1;
});

//storage
app.ports.save.subscribe((storage) => {
  localStorage.setItem("storage", JSON.stringify(storage));
  app.ports.load.send(storage);
});

// Handle smoothly scrolling to links
const scrollToHash = () => {
  const BREAKPOINT_XL = 1920;
  const NAVBAR_HEIGHT_PX = window.innerWidth > BREAKPOINT_XL ? 127 : 102;
  const element = window.location.hash && document.querySelector(window.location.hash);
  localStorage.setItem("storage", localStorage.getItem("storage").replace('"mobileNav":true', '"mobileNav":false'));
  if (element) {
      // element.scrollIntoView({ behavior: 'smooth' })
      window.scroll({ behavior: "smooth", top: window.pageYOffset + element.getBoundingClientRect().top - NAVBAR_HEIGHT_PX });
  } else {
      window.scroll({ behavior: "auto", top: 0 });
  }
  app.ports.load.send(JSON.parse(localStorage.getItem("storage")));
};

app.ports.onUrlChange.subscribe((_) => {
  enableRecvScroll = false;
  setTimeout(scrollToHash, 400);
  enableRecvScroll = true;
});