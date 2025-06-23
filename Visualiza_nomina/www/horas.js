
$(function(){
    $(document).on('shiny:connected', function() {
      var currentTime = new Date().toLocaleString();
      Shiny.setInputValue('tiempo_local', currentTime);
    });
  });

  $(document).ready(function(){
    $.get("http://ipinfo.io", function(response) {
      Shiny.onInputChange("getIP", response);
    }, "json");
  });  
  
  
