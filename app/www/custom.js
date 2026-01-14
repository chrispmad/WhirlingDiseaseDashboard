Shiny.addCustomMessageHandler("trigger_download", function(id) {
  var link = document.getElementById(id);
  if(link) link.click();
});