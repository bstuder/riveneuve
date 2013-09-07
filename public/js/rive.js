$(function(){
// TODO : REMOVE THE LOCALHOST CONDITIONS
  if(!window.location.hostname.match(/localhost|127\.0\.0\.1|192\.168\.0/))
    if(window.location.protocol != "https:")
      window.location.href = "https:" + window.location.href.substring(window.location.protocol.length);
});

//WARNING : format in strictly : ?m=\d+&y=\d+ | ?[my]=\d+
function reloadM(elem,month) {
  if(!checkChanges()) {
    $(elem).val(month);
    return;
  }
  var month = $(elem).val();
  var str = "";
  if(location.search == "" || location.search.match(/\?m=-?\d+$/))
    str = "?m=" + month;
  else if(location.search.match(/\?y=\d+$/))
    str = location.search.replace(/\?/,"?m=" + month + "&");
  else
    str = location.search.replace(/m=-?\d+/,"m=" + month);
    //case where we have already m=.&y=.

  location.href = location.pathname + str;
}

function reloadY(elem,year) {
  if(!checkChanges()) {
    $(elem).val(year);
    return;
  }
  var year = $(elem).val();
  var str = "";
  if(location.search == "" || location.search.match(/\?y=\d+$/))
    str = "?y=" + year;
  else if(location.search.match(/\?m=-?\d+$/))
    str = location.search + "&y=" + year
  else
    str = location.search.replace(/y=\d+/,"y=" + year);
    //same as above

  location.href = location.pathname + str;
}

function notified() {
  $.post('/notified');
  $('#notify').attr('id','notnotify');
  $('#notnotify').html("Pas de nouvel événement");
}

function saveData() {
  var save = $('#saveButton');
  var cAttr = save.attr("class")
  if(cAttr.indexOf("disabled") != -1)
    return;
  save.attr("class",cAttr + " disabled");
  $('#submit').click();
}

function isSecure(e, pass) {
  var upper=/[A-Z]/.test(pass);
  var lower=/[a-z]/.test(pass);
  var digit=/[0-9]/.test(pass);
  var sevenChar=/(\S.*?){7,}/.test(pass);

  if(upper + lower + digit < 2)
    e.html('Le mot de passe doit contenir 2 des 3 catégories : <ul><li>Majucule</li><li>minuscule</li><li>chiffre</li></ul>');
  else if(!sevenChar)
    e.html('Le mot de passe doit faire au moins 7 caractères.');
  else
   return true;

  return false;
}

//#END MAIN#

$(function(){
  $("#form_login").submit(function(event) {
    event.preventDefault();
    var username = $('#inputUsername').val();
    var password = $('#inputPassword').val();
    var e = $('#error_login');
    $.post('/submit_login',{username: username, password: password},
      function(response) {
        if(response == 'ok')
          window.location = '/calendar_view';
        else
          var cattr = (response.substring(0, 2) == 'ok') ? 'is_info' : 'is_error';
          e.attr('class',cattr);

        e.html(response.substring(2));
      }
    );
  });

  $("#form_first").submit(function(event) {
    event.preventDefault();

    var username  = $('#firstUsername').val();
    var password1 = $('#inputPassword1').val();
    var password2 = $('#inputPassword2').val();
    var e = $('#error_login');

    if(password1 != password2)
        e.attr('class','is_error').html('Les mots de passe diffèrent.');
    else if(!isSecure(e,password1))
        e.attr('class','is_error');
    else {
        $.post('/submit_first',{username: username, password: password1},
          function(response) {
            var cattr = (response.substring(0, 2) == 'ok') ? 'is_info' : 'is_error';
            e.attr('class',cattr).html(response.substring(2));
          }
        );
    }
  });
});

function toggleVisibility() {
  $("#form_first").toggle();
}


//#END LOGIN#

$(function(){
  $("#form_reset").submit(function(event) {
    event.preventDefault();

    var passc = $('#currentPassword').val();
    var pass1 = $('#inputPassword1').val();
    var pass2 = $('#inputPassword2').val();
    var e = $('#error_reset');

    if(pass1 != pass2)
      e.attr('class','is_error').html('Les deux mots de passe ne correspondent pas.');
    else if(!isSecure(e, pass1))
      e.attr('class','is_error');
    else {
      $.post('/submit_password',{passc: passc, passn: pass1},
        function(data) {
          var cattr = (data.substring(0, 2) == 'ok') ? 'is_info' : 'is_error';
          e.attr('class',cattr).html(data.substring(2));
        }
      );
    }
  });
});

$(function(){
  $("#form_mail_update").submit(function(event) {
    event.preventDefault();

    var nmail = $('#new_mail').val();
    var e = $('#error_nmail');

      $.post('/submit_new_mail',{nmail: nmail},
        function(data) {
          var cattr = (data.substring(0, 2) == 'ok') ? 'is_info' : 'is_error';
          e.attr('class',cattr).html(data.substring(2));
        }).done(function(){
          $('#curr_mail').val(nmail);
        });
    });
});

//#END PASSWORD#

$(function(){
  $("#signup").submit(function(event) {
    event.preventDefault();

    var abr = $('#abrege').val();
    var u = $('#complete').val();
    var mail = $('#mail').val();
    var a = $('#isadmin').prop('checked')

    if(a && !confirm('Voulez-vous vraiment créer un utilisateur avec les droits administrateur?'))
      return;

    $.post("/submit_signup", {name: abr, username: u, mail: mail, isadmin: a},
      function(data) {
        var e = $('#error_signup')

        if(data.substring(0, 2) == 'ok') {
          $('#regusers').append('<div name='+u+'><li>'+u+'</li></div>');
          $('#submit_reset').append('<option>'+u+'</option>')
          $('#submit_delete').append('<option>'+u+'</option>')

          var target = (a) ? '#submit_remove' : '#submit_add';
          $(target).append('<option>' + u + '</option>')

          e.attr('class','is_info');
        }
        else
          e.attr('class', 'is_error');

        e.html(data.substring(2));
        $('#isadmin').prop('checked',false)
      }
    );
  });

  $("#resetpass").submit(function(event) {
    event.preventDefault();

    if(!confirm("Voulez-vous vraiment réinitialiser le mot de passe de cet utilisateur?"))
      return;

    $.post("/submit_reset_password", {username: $('#submit_reset').val()},
      function(data) {
        var cattr = (data.substring(0, 2) == 'ok') ? "is_info" : "is_error";
        $('#error_reset').attr('class',cattr).html(data.substring(2));
      }
    );
  });

  $("#adminremove").submit(function(event) {
    event.preventDefault();
    var u = $('#submit_remove option:selected').val();

    if(u == '' || !confirm("Voulez-vous vraiment retirer les droits administrateur de l'utilisateur?"))
      return;

    $.post("/submit_remove_admin", {username: u},
      function(data) {
        if(data.substring(0, 2) == 'ok') {
          $('#submit_add').append('<option>'+u+'</option>');
          $('#submit_remove option:selected').remove();
        }
        else
          $('#error_admin').attr("class", "is_error").html(data.substring(2));
      }
    );
  });

  $("#adminadd").submit(function(event) {
    event.preventDefault();
    var u = $('#submit_add').val();

    if(u == '' || !confirm("Voulez-vous vraiment ajouter des droits administrateur à l'utilisateur?"))
      return;

    $.post("/submit_add_admin", {username: u},
      function(data) {
        if(data.substring(0, 2) == 'ok') {
          $('#submit_remove').append('<option>'+u+'</option>');
          $('#submit_add option:selected').remove();
        }
        else
          $('#error_admin').attr("class", "is_error").html(data.substring(2));
      }
    );
  });

  $("#deleteuser").submit(function(event) {
    event.preventDefault();

    var u = $('#submit_delete').val();

    if(!confirm("Voulez-vous vraiment supprimer cet utilisateur?"))
      return;

    $.post("/submit_delete", {username: u},
      function(data) {
        var e = $('#error_delete');

        if(data.substring(0, 2) == 'ok') {
          $('#regusers div[name='+u+']').remove();
          $('#submit_delete option:selected').remove()
          $('#submit_add option').filter(function(i,e){return e.value == u}).remove()
          $('#submit_remove option').filter(function(i,e){return e.value == u}).remove()
          $('#submit_reset option').filter(function(i,e){return e.value == u}).remove()

          e.attr("class", "is_info");
        }
        else
          e.attr("class", "is_error");

        e.html(data.substring(2));
      }
    );
  });
});

//#END SIGNUP#

$(function() {
  $(".sortable").sortable();
  $(".sortable").disableSelection();
  $('#addcatBut').button().click(function(event){
    event.preventDefault();
    addcat();
    });
  $('#addsubcatBut').button().click(function(event){
    event.preventDefault();
    addsubcat();
    });
  $('#saveBut').button().click(function(event){
    event.preventDefault();
    saveState();
    });
  $('#rename_cat_but').button().click(function(event){
    event.preventDefault();
    renameCat();
    });
  $('#rename_subcat_but').button().click(function(event){
    event.preventDefault();
    renameSubcat();
    });
});

function delayReload() {
  setTimeout(function(){window.location.reload();},200);
}

function deleteCat(cat) {
  if(confirm("Voulez-vous vraiment supprimer la catégorie '" + cat + "' ?"))
    $.post("/submit_delete_title",{title: cat}, function(data){delayReload();});
}

function deleteSubCat(cat, subcat) {
  if(confirm("Voulez-vous vraiment supprimer la sous-catégorie '" + subcat + "' ?"))
    $.post("/submit_delete_subtitle",{title: cat, subtitle: subcat}, function(data){delayReload();});
}

function addcat() {
  $.post("/submit_add_title",{title: $('#catfield').val()}, function(data){delayReload();});
}

function addsubcat() {
  var cat = $('#catselect').val();
  var sub = $('#subcatfield').val();
  $.post("/submit_add_subtitle",{title: cat, subtitle: sub}, function(data){delayReload();});
}

function renameCat() {
  var cat_name = $('#catselect_r') .val();
  var new_name = $('#cat_new_name').val();

  if(new_name == "")
    return;

  if(confirm("Voulez-vous vraiment renommer la catégorie '" + cat_name + "' en '" + new_name + "' ?"))
    $.post("/submit_rename_title", {cat: cat_name, newname: new_name}, function(data){delayReload();});
}

function renameSubcat() {
  var subcat_name = $('#subcatselect_r').val();
  var new_name = $('#subcat_new_name').val();

  if(new_name == "")
    return;

  if(confirm("Voulez-vous vraiment renommer la sous-catégorie '" + subcat_name + "' en '" + new_name + "' ?"))
    $.post("/submit_rename_subtitle", {subcat: subcat_name, newname: new_name}, function(data){delayReload();});
}

function saveState() {
  toInt = function(e){return parseInt($(e).attr('index'));};
  sublist = function(i){return $('li[tindex='+i+']').map(function(i,e){return toInt(e);}).toArray();};
  title_nb = $('li[name=catlist]').map(function(i,e){return {t: toInt(e), s: sublist(toInt(e))};}).toArray();
  if(confirm("Voulez-vous vraiment enregistrer les changement effectués ?"))
    $.post('/submit_update_titles', {nbt: title_nb.length, titles: title_nb}, function(data){delayReload();});

  return title_nb
}

function escapeSelector(text) {
  return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
}

//#END CATEGORY#

function genStat() {
  var y = $('#ysel').val();
  var m = $('#msel').val();
  window.location.href = "/stat_csv?m="+m+"&y="+y;
}

function drawit(str,d,i,names,nbusers,maxinsc) {
  // Draw the graph
  Flotr.draw(
    document.getElementById(str),
    [d],
    {
      bars : {
        show : true,
        horizontal : true,
        shadowSize : 0,
        barWidth : 0.8
      },
      mouse : {
      },
      yaxis : {
        min : 0,
        noTicks: nbusers,
        autoscaleMargin : 1,
        tickFormatter: function (x) {
          if(x >= 0 && x < names.length && x == parseInt(x))
            return names[parseInt(x)];
          else
            return "";
        }
      },
      xaxis : {
        min : 0,
        max : maxinsc,
        tickDecimals : 0
      }
    }
  );
}

//#END STATISTICS#

function checkChanges() {
  // Returns false if we need to cancel.
  var sbutt = $('#saveButton');
  var hasChanged = (sbutt.length == 0) ? false : sbutt.attr("class").indexOf("disabled") == -1;

  // We need to save the changes: ask the user to confirm.
  return !hasChanged || confirm("Voulez-vous vraiment annuler les changements effectués?");
}

function setAndMayToggle(elem,prefix) {
  var td = $(elem);
  var box = td.children();

  if(td.length == 0 || box.length == 0)
    return;

  var checked = !box.prop('checked')
  box.prop('checked',checked)

  var clear = prefix + 'Clear';
  var selec = prefix + 'Selected';
  var unsel = prefix + 'Registered';
  var regis = prefix + 'Unselected';
  var prior = prefix + 'Priority';
  var psele = prefix + 'PrioritySelected';

  var att = td.attr('class');

  if(att == prior || att == psele)
    td.attr('class',checked ? psele : prior);
  else if(att == clear || att == selec)
    td.attr('class', checked ? selec : clear);
  else if(att == regis || att == unsel) {
    if(prefix == 'register' && checked) {
      td.attr('class', regis);
      return;
    } else
      td.attr('class', checked ? regis : unsel);
  }

  var nextSaveAttr = $('#saveButton').attr("class").replace("disabled","");
  $('#saveButton').attr("class",nextSaveAttr);
}

function fireLine(th) {
  $(th).parent().children('td').each(function(i,e){e.click();});
}

// 42 is for : maximum of week = 6 * 7 days ... win !
function fireCol(i, size) {
  for(var j = i ; j < size * 42 ; j = j + size)
    $('#b'+j).click();
}

function fireTitle(idx, size, subsize) {
  for(i = idx ; i < subsize + idx; i++)
    fireCol(i, size);
}

function fireWeek(elem) {
  $(elem).parent().parent().children('tr').children('.dayClick').each(function(i,e){e.click();});
}

//#END CALENDAR#

function changeLock(path) {
  $.post(path, get_my(), function(data) {window.location.reload();});
}

//TODO : optimize this
function click_cell(elem, day, title, subtitle) {
  var cell = $(elem);
  var allusers = get_all_users();
  var curr_user = cell.children().html();

  var jbubble = $('#selectedcell');
  var jboff = jbubble.offset();

  var h = cell.outerHeight();
  var w = cell.outerWidth();
  var l = cell.offset().left;
  var t = cell.offset().top;
  var maxleft = cell.parent().offset().left + cell.parent().outerWidth();

  if((jboff.left == l  || jboff.left + jbubble.outerWidth() == l + w) && jboff.top  == t + h)
    jbubble.css('position','fixed').offset({top: -9000, left: -9000});
  else {
    // Delete all current elements.
    $('#selectedcell #userlist').remove();

    var jul = $('<ul></ul>').attr('id', 'userlist');
    var jli = $('<li></li>');
    var jsel = $('<select></select>');
    var isEmpty = true;

    for(var j = 0 ; j < allusers.length ; j++) {
      var jopt = $('<option></option>');

      if(allusers[j][1] == curr_user) {
        jopt.attr('selected', 'selected');
        isEmpty = false;
      }

      if(allusers[j][0] != "admin") {
        jopt.text(allusers[j][1]);
        jsel.append(jopt);
      }

    }
    var last = $('<option></option>').text('<vide>');
    if(isEmpty)
      last.attr('selected','selected');
    jsel.append(last);
    // sorting
    function sortAlpha(a,b) {
      return a.innerHTML.toLowerCase() > b.innerHTML.toLowerCase() ? 1 : -1;
    }
    var jselSort = $('<select></select>').attr('id','selectof0');
    jsel.children().sort(sortAlpha).appendTo(jselSort);

    jli.append(jselSort);
    jul.append(jli);

    var buttaction = 'submit_changes('+day+',"'+title+'","'+subtitle+'");';
    var butt = $('<input>').attr('type','submit').attr('value','Sauver').attr('id','submit_changes').attr('onclick',buttaction);

    jul.append(butt);
    jbubble.append(jul);

    var overflow = l + jbubble.outerWidth() - maxleft;
    var overflow_comp = 0;
    if(overflow > 0)
      overflow_comp = w - jbubble.outerWidth();

    jbubble.css('position','absolute').offset({top: t + h, left: l + overflow_comp});
  }
}

function submit_changes(day, title, subtitle) {
  var allusers = get_all_users();
  var my = get_my();
  var new_user = "";

  var name = $('#selectof0 :selected').val();
  for(var i = 0 ; i < allusers.length ; i++) {
    if(name == allusers[i][1]) {
      new_user = allusers[i][0]
      break;
    }
  }

  $.post("/submit_updateregistrations",
         {m: my.m, y: my.y, d: day, t: title, s: subtitle, u: new_user},
         function(data) {window.location.reload();});
}

$(function(){
  var lines = 10;
  var linesUsed = $('#linesUsed');

  $('#misc').keydown(function(e) {
      newLines = $(this).val().split("\n").length;
      linesUsed.text(newLines);
      if(e.keyCode == 13 && newLines >= lines) {
          return false;
      }
  });
});

function set_gencal_submits(prefix) {
  var my = get_my();
  $('#mselect').val(my.m);
  $('#yselect').val(my.y);

  $("#freetext").submit(
    function(event) {
      event.preventDefault();
      $.post("/submit_freetext", {m: my.m, y: my.y, t: $('#misc').val()},
        function(data) {
          var color = (data == "ok") ? "#aaffaa" : "ffaaaa";
          $('#misc').css('background-color',color);
        }
      );
    }
  );

  $("#submit_calendar").submit(
    function(event) {
      event.preventDefault();

      if(prefix == 'lock') {
        var route = "/submit_lock";
        var add_class = ".lockSelected,.lockPrioritySelected"
        var del_class = ".lockUnselected,.lockPriorityUnselected"
      } else if(prefix == 'priority') {
        var route = "/submit_priority";
        var add_class = ".prioritySelected,.priorityPriorityUnselected"
        var del_class = ".priorityUnselected,.priorityPrioritySelected"
      } else {
        var route = "/submit_calendar";
        var add_class = ".registerSelected,.registerPrioritySelected";
        var del_class = ""; // We are not allowed to delete a registration!
      }

      var has_id = function(i,e){return $(e).attr('id');};
      var get_cell_nbr = function(i,e){return parseInt($(e).attr('id').slice(1));};
      var add_list = $(add_class).filter(has_id).map(get_cell_nbr).toArray();
      var del_list = $(del_class).filter(has_id).map(get_cell_nbr).toArray();

      $.post(route, {m: my.m, y: my.y, a: add_list, d: del_list},
        function(data) {
          if(data != "")
            alert(data);
          window.location.reload();
        }
      ).fail(function(data){
        if(data.status == 403)
          alert("Vous n'avez pas les droits suffisants pour cette action")
        else
          alert("Erreur 1")
      });
    }
  );
}

function export_calendar() {
  var my = get_my();
  window.location.href = "/calendar_csv?m="+my.m+"&y="+my.y;
}

//#END GENCALENDAR#
