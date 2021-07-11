document.addEventListener("keydown", keyDownTextField, false);

function keyDownTextField(e) {
var keyCode = e.keyCode.toString();
window.location.href = "/?key=" + keyCode;
}