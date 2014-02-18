function numberchange(inputText, pluralId) {
  var setting = inputText === '1' ? 'hidden' : 'visible';
  document.getElementById(pluralId).style.visibility = setting;
}
