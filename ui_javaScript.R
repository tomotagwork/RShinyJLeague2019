
strJavaScript01 <- "

  function openTab_MainMenu(strMainMenu) {
    $('a', $('.sidebar')).each(function(){
      var spanElement=this.getElementsByTagName('span')[0]
      if (spanElement) {
        var spanString=spanElement.textContent
        //alert(spanString)
        if (spanString == strMainMenu){
          this.click()
        }
      }
    })
  }
  
  function openTab_SubMenu(strSubMenu) {
    $('a', $('.sidebar')).each(function() {
      if(this.getAttribute('data-value') == strSubMenu) {
        this.click()
      }
    })
  }


  function getUrlHash(){
    var hash = window.location.hash.slice(1);
    return hash;
  }

  function getUrlVars(){
    var vars = [], max = 0, hash = '', array = ''
    var query = window.location.search.slice(1).split('&')
    max = query.length

    for (var i = 0; i < max; i++) {
      array = query[i].split('=')
      vars.push(array[0]);
      vars[array[0]] = array[1]
    }
    return vars
  }

  $(document).on('shiny:connected', function(event) {
    var hash = getUrlHash()

    if(hash == 'shiny-tab-tab_Point') {
      openTab_MainMenu('勝点推移')
      
    } else if (hash == 'shiny-tab-tab_ResultAll') {
      openTab_MainMenu('日程/結果')
      openTab_SubMenu('tab_ResultAll')
      
    } else if (hash == 'shiny-tab-tab_ResultTeam') {
      openTab_MainMenu('日程/結果')
      openTab_SubMenu('tab_ResultTeam')
      
    }

  })

"
