package controllers

import play.api.mvc.{Controller, Action, SimpleResult, ResponseHeader}
import play.api.libs.iteratee.Enumerator
import anorm.SQL // sql
import anorm.SqlParser.{str, scalar} //sql parser
import com.roundeights.hasher.Implicits.stringToHasher // String to sha256
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.apache.poi.ss.usermodel.{Row, Cell => XCell, CellStyle, IndexedColors}
import org.apache.poi.hssf.usermodel.HSSFWorkbook 

import Utils._

object Posts extends Controller {

  val GUEST_USR = "guest"
  val GUEST_PASS = "guest-rive"

  // Burn the cookie.
  // Clean his temporary salt.
  def submit_logout = Action { implicit request =>
    Redirect("/").withNewSession
  }

  def isGuestLog(query: Map[String,Seq[String]]): Boolean = {
    val username = query("username")(0) 
    val pass = query("password")(0)

    username == GUEST_USR && pass == GUEST_PASS 
  }

  def submit_login = Action { implicit request =>
    if(isLogged(session))
      Ok("ok")
    else if(!checkRequest("username", "password")())
      BadRequest(INVALARG)
    else if(isGuestLog(request.body.asFormUrlEncoded.get))
      Ok("ok").withSession("username" -> GUEST_USR)
    else {
      val query = request.body.asFormUrlEncoded.get
      val username = query("username")(0) 
      val pass = query("password")(0)

      transaction(Committed()){ implicit conn =>
        try {
          val hashOpt = SQL(
            """
              select password from users
              where username={u}
            """
          ).on("u" -> username).as(str("password") singleOpt)

          val saltOpt = SQL(
            """
              select salt from users
              where username={u}
            """
          ).on("u" -> username).as(str("salt") singleOpt)

          if(hashOpt.isEmpty || saltOpt.isEmpty) 
            Ok(ERRLOGIN)
          else {
            if(hashOpt.get == (pass + saltOpt.get).sha256.toString)
              Ok("ok").withSession("username" -> username)
            else
              Ok(ERRLOGIN)
          }
        } catch {
          case _ => Ok("erContactez l'Administrateur (erreur: log).") 
        }
      }(Ok("er"))
    }
  }

  // By default, when an account is created, not password is attached to it.
  // When the user first logs in, he submits his password by using this function,
  // which is the unsafest function of the registration process.
  def submit_first = Action { implicit request =>
    if(!checkRequest("username","password")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val username = query("username")(0)
      val pass = query("password")(0)
      val salt = randHash()
      val hash = (pass + salt).sha256.toString
  
      if(!userExists(username))
        Ok(ERRFIRST)
      else {
        transaction(Atomic()){ implicit conn =>
          try { 
            val hashOpt = SQL(
              """
                select password from users
                where username={u}
              """
            ).on("u" -> username).as(str("password") singleOpt)
            
            if(hashOpt.isEmpty || hashOpt.get != "") {
              conn.rollback
              conn.commit
              Ok(ERRFIRST)
            } else {
              SQL(
                """
                  update users
                  set password={p},salt={s}
                  where username={u}
                """
              ).on(
                "u" -> username,
                "p" -> hash,
                "s" -> salt
              ).execute()
    
              conn.commit
              Ok("okLe mot de passe a bien été défini.")
            }
          }
          catch {
            case _ => {
              conn.rollback
              conn.commit
              Ok("erContactez l'Administrateur (erreur: log_fst).") 
            }
          }
        }(Ok("er"))
      }
    }
  }

  def submit_rename_title = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("cat", "newname")())
      BadRequest(INVALARG)
    else {
      val query   = request.body.asFormUrlEncoded.get
      val cat     = query("cat")(0)
      val newname = sanatize_input(query("newname")(0))

      if(isempty(newname))
        Ok("er")
      else {
        transaction(Atomic()){ implicit conn =>
          try {
            // must add the new title first to resolve foreign key
            SQL(
              """
                insert into titles (title)
                values ({nt})
              """
            ).on(
              "nt" -> newname
            ).execute()

            SQL(
              """
                update activities
                set title={nt}
                where title={ot}
              """
            ).on(
              "ot" -> cat,
              "nt" -> newname
            ).execute()

            // and remove the title once switch it's done
            SQL(
              """
                delete from titles
                where title={ot}
              """
            ).on(
              "ot" -> cat 
            ).execute()

            SQL(
              """
                update registrations
                set title={nt}
                where title={ot}
              """
            ).on( 
              "ot" -> cat,
              "nt" -> newname
            ).execute()

            SQL(
              """
                update locked 
                set title={nt}
                where title={ot}
              """
            ).on( 
              "ot" -> cat,
              "nt" -> newname
            ).execute()

            SQL(
              """
                update priority 
                set title={nt}
                where title={ot}
              """
            ).on( 
              "ot" -> cat,
              "nt" -> newname
            ).execute()

            conn.commit

            regenTitles()

            Ok("ok")
          }
          catch {
            case err => {
              conn.rollback
              conn.commit

              println("rollback")
              println(err)

              Ok("er")
            }
          }
        }(Ok("er"))
      }
    }
  }

  def submit_rename_subtitle = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("subcat", "newname")())
      BadRequest(INVALARG)
    else {
      val query   = request.body.asFormUrlEncoded.get
      val subcat  = query("subcat")(0)
      val optcat  = titles.find(_._2.contains(subcat)) 
      val newname = sanatize_input(query("newname")(0))

      if(isempty(newname) || optcat == None)
        Ok("er")
      else {
        val cat = optcat.get._1
        transaction(Atomic()){ implicit conn =>
          try {
            SQL(
              """
                update activities
                set subtitle={ns}
                where  title={t}
                and subtitle={os}
              """
            ).on(
              "t"  -> cat,
              "os" -> subcat,
              "ns" -> newname
            ).execute()

            SQL(
              """
                update registrations
                set subtitle={ns}
                where  title={t}
                and subtitle={os}
              """
            ).on( 
              "t"  -> cat,
              "os" -> subcat,
              "ns" -> newname
            ).execute()

            SQL(
              """
                update locked 
                set subtitle={ns}
                where  title={t}
                and subtitle={os}
              """
            ).on( 
              "t"  -> cat,
              "os" -> subcat,
              "ns" -> newname
            ).execute()

            SQL(
              """
                update priority 
                set subtitle={ns}
                where  title={t}
                and subtitle={os}
              """
            ).on( 
              "t"  -> cat,
              "os" -> subcat,
              "ns" -> newname
            ).execute()

            conn.commit

            regenTitles()

            Ok("ok")
          }
          catch {
            case err => {
              conn.rollback
              conn.commit

              println("rollback")
              println(err)

              Ok("er")
            }
          }
        }(Ok("er"))
      }
    }
  }

  def submit_calendar = Action { implicit request =>
    if(!isLogged(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("m", "y"))
      BadRequest(INVALARG)
    else {
      var success = false

      val query = request.body.asFormUrlEncoded.get
      val month = query("m")(0).toInt
      val year = query("y")(0).toInt
      
      val add_indexes = req2listint(query.get("a[]"))

      val tList = titles.toList
      val subtList = tList.foldLeft(Nil:List[String])(_ ::: _._2)
      val tsize = (titles.size, tList.map(_._2.size).sum)
      val ctab = new Table(month, year) 
  
      val username = getStat(session) match {
        case User(user)  => user
        case Admin(user) => user
        case _           => "" 
      }
      
      var error_str = ""
  
      for(i <- add_indexes) {
        val subidx = i % tsize._2
        val subtitle = subtList(subidx)
        val title = tList.find(_._2.contains(subtitle)).get._1 
        val day = 1 + i / subtList.size - ctab.prevDays.size
         
        if( year <  date("y")
        || (year == date("y") && month <  date("m"))
        || (year == date("y") && month == date("m") && day < date("d"))) {
          error_str = error_str + "L'enregistrement pour '" + 
                          title + "/" + subtitle + "' le " + day + 
                          " du mois est impossible: la date est antérieure.\n"
        } else {
          transaction(Atomic()){ implicit conn =>
            try {
              SQL(
                """
                  insert into registrations (year, month, day, title, subtitle, username)
                  values ({y}, {m}, {d}, {t}, {s}, {u})
                """
              ).on(
                "y" -> year,
                "m" -> month,
                "d" -> day,
                "t" -> title,
                "s" -> subtitle,
                "u" -> username 
              ).execute()
  
              val count = SQL(
                """
                  select count(*) from registrations
                  where    year={y} and
                          month={m} and
                            day={d} and
                          title={t} and
                       subtitle={s}
                """
              ).on(
                "y" -> year,
                "m" -> month,
                "d" -> day,
                "t" -> title,
                "s" -> subtitle
              ).as(scalar[Long] single)
          
              // If there is more than one registration with
              // the same day, category and subcategory
              if(count > 1) {
                error_str = error_str + "L'enregistrement pour '" + 
                               title + "/" + subtitle + "' le " + day + 
                               " du mois est impossible: toutes les places sont prises.\n"

                conn.rollback
                conn.commit
              }
              else {
                success = true
                conn.commit
              }
            }
            catch {
              case _ => {
                error_str = error_str + "L'enregistrement pour '" + 
                                title + "/" + subtitle + "' le " + day + 
                                " du mois existe déjà.\n"

                conn.rollback
                conn.commit
              }
            }
          }()
        }
      }

      if(success) {
        val querystr = """
          update params
          set value={v}
          where param={f}
        """
        
        transaction(Atomic()){ implicit conn =>
          try {
            var batch = SQL(querystr).asBatch 

            val update_field: (String, Int) => Unit = (field, value) => {
              batch = batch.addBatch('f.name -> field, 'v.name -> value)
            }

            update_field("user_y"  , date("y"))
            update_field("user_m"  , date("m"))
            update_field("user_d"  , date("d"))
            update_field("user_h"  , date("h"))
            update_field("user_min", date("min"))
            update_field("user_sec", date("sec"))
        
            batch.execute()

            conn.commit
          }
          catch {
            case _ => {
              error_str = error_str + "Contactez l'Administrateur (erreur: cal_sub)."
              conn.rollback
              conn.commit
            }
          }
        }()
      }

      Ok(error_str) 
    }
  }
 
  def submit_notified = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else {
      transaction(Atomic()){ implicit conn =>
        try {
          val querystr = """
            update params
            set value={v}
            where param={f}
          """
        
          var batch = SQL(querystr).asBatch 

          val update_field: (String, Int) => Unit = (field, value) => {
            batch = batch.addBatch(
              'f.name -> field, 'v.name -> value)
          }

          update_field("admin_y"  , date("y"))
          update_field("admin_m"  , date("m"))
          update_field("admin_d"  , date("d"))
          update_field("admin_h"  , date("h"))
          update_field("admin_min", date("min"))
          update_field("admin_sec", date("sec"))

          batch.execute()

          conn.commit

          Ok("success")
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit

            Ok("Contactez l'Administrateur (erreur: notify_sub).")
          }
        }
      }(Ok("er"))
    }
  }

  def submit_password = Action { implicit request =>
    if(!isLogged(session))
      Ok("erVous devez être authentifié!")
    else if(!checkRequest("passc","passn")())
      BadRequest(INVALARG)
    else {
      val username = getStat(session) match {
        case Admin(u) => u
        case User(u)  => u
      }

      val query = request.body.asFormUrlEncoded.get
      val passc = query("passc")(0)
      val passn = query("passn")(0)
      val saltn = randHash()

      transaction(Atomic()){ implicit conn =>
        try {
          val dbhash = SQL(
            """
              select password from users
              where username={u}
            """
          ).on(
            "u" -> username
          ).as(str("password") single)

          val saltc = SQL(
            """
              select salt from users
              where username={u}
            """
          ).on("u" -> username).as(str("salt") single)

          if((passc + saltc).sha256.toString != dbhash) {
            conn.rollback
            conn.commit

            Ok("erMot de passe invalide.")
          }
          else {
            SQL(
              """
                update users
                set password={p},salt={s}
                where username={u}
              """
            ).on(
              "u" -> username,
              "p" -> (passn + saltn).sha256.toString,
              "s" -> saltn
            ).execute()

            conn.commit
            
            Ok("okMot de passe changé avec succès.")
          }
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit

            Ok("erContactez l'Administrateur (erreur: pass_sub).")
          }
        }
      }(Ok("er"))
    }
  }

  def stat_csv(m: Int, y: Int) = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(m > 12)
      BadRequest(INVALARG)
    else {
      val (m_opt,m_str) = if (m <= 0) (None,"total") else (Some(m),m.toString)
      val statmap = getStatMap(y,m_opt)
      val out = new ByteArrayOutputStream();
      val wb = new HSSFWorkbook()
      val s = wb.createSheet()
      var r :Row = null
      var c :XCell = null

      val cs = wb.createCellStyle()
      cs.setAlignment(CellStyle.ALIGN_CENTER)

      r = s.createRow(0)
      r.createCell(0)
      for (i <- 0 until titles.size) {
        c = r.createCell(i+1)
        c.setCellStyle(cs)
        c.setCellValue(titles(i)._1)
      }

      for(i <- 0 until allUsers.size) {
        r = s.createRow(i+1)

        c = r.createCell(0)
        c.setCellStyle(cs)
        c.setCellValue(allUsers(i))

        for(j <- 0 until titles.size) {
          c = r.createCell(j+1)
          c.setCellStyle(cs)
          c.setCellValue(statmap(titles(j)._1)(allUsers(i)))
        }
      }
      
      r = s.createRow(allUsers.size+2)
      c = r.createCell(0)
      c.setCellStyle(cs)
      c.setCellValue("Total")
      for (i <- 0 until titles.size){
        c = r.createCell(i+1)
        c.setCellStyle(cs)
        c.setCellValue(statmap(titles(i)._1).map(_._2).sum)
      }

      for (i <- 0 to titles.size)
        s.autoSizeColumn(i)

      wb.setSheetName(0, "statistics")

      wb.write(out);
      out.close();

      val enum = Enumerator.fromStream(new ByteArrayInputStream(out.toByteArray))
      val name = "statistics-"+m_str+"-"+y+".xls"

      SimpleResult(
        header = ResponseHeader(OK, Map(
                  //CONTENT_LENGTH -> content.length.toString,
                  CONTENT_TYPE -> play.api.libs.MimeTypes.forFileName(name).getOrElse(play.api.http.ContentTypes.BINARY),
                  CONTENT_DISPOSITION -> ("""attachment; filename="%s""".format(name)))
                  ),
        body = enum 
      )
    }
  }

  def calendar_csv(m: Int, y: Int) = Action { implicit request =>
    if(!(isLogged(session) || isGuest(session)))
      Forbidden(views.html.forbidden())
    else if(m > 12 || m < 0)
      BadRequest(INVALARG)
    else {
      val spacedTitles = titles.foldLeft(Nil:List[String])((b,t) => b ::: List(t._1) ::: List.fill(t._2.size-1)(""))
      val subtitles = titles.foldLeft(Nil:List[String])(_ ::: _._2)
      val mtable = genTable(m,y)
      val tsize = titles.foldLeft(0)(_ + _._2.size)
      val out = new ByteArrayOutputStream() 
      val wb = new HSSFWorkbook()
      val s = wb.createSheet()
      var r :Row = null
      var c :XCell = null
      var row_nbs = 0

      val cs_reg = wb.createCellStyle()
      val cs_lock = wb.createCellStyle()
      val cs_prio = wb.createCellStyle()
      val cs_title = wb.createCellStyle()
      val cs_subtitle = wb.createCellStyle()

      //val f = wb.createFont()
      //set font 1 to 12 point type
      //f.setFontHeightInPoints(12)
      //make it blue
      //f.setColor(0xc)
      // make it bold
      //arial is the default font
      //f.setBoldweight(Font.BOLDWEIGHT_BOLD)

      cs_reg.setFillPattern(CellStyle.SOLID_FOREGROUND)
      cs_reg.setFillForegroundColor(IndexedColors.LIGHT_GREEN.index)
      cs_reg.setAlignment(CellStyle.ALIGN_CENTER)

      cs_lock.setFillPattern(CellStyle.SOLID_FOREGROUND)
      cs_lock.setFillForegroundColor(IndexedColors.GREY_40_PERCENT.index)

      cs_prio.setFillPattern(CellStyle.SOLID_FOREGROUND)
      cs_prio.setFillForegroundColor(IndexedColors.LIGHT_YELLOW.index)

      cs_title.setAlignment(CellStyle.ALIGN_CENTER)
      
      //titles
      r = s.createRow(row_nbs)
      row_nbs += 1
      c = r.createCell(0)
      c.setCellStyle(cs_title)
      c.setCellValue(MONTHS(mtable.m-1)._2)
      for (i <- 0 until spacedTitles.size){
        c = r.createCell(i+1)
        c.setCellStyle(cs_title)
        c.setCellValue(spacedTitles(i))
      }
      //subtitles
      r = s.createRow(row_nbs)
      row_nbs += 1
      r.createCell(0)
      for (i <- 0 until subtitles.size) {
        c = r.createCell(i+1)
        c.setCellStyle(cs_title)
        c.setCellValue(subtitles(i))
      }

      val fday = mtable.firstDayOfMonth
      val reg_map = get_registered_users_map(mtable.m, mtable.y)
      for (i <- 1 to mtable.nbrOfDays) {
        if((fday + i - 1) % 7 == 0 && i > 0) {
          r = s.createRow(row_nbs+1)
          row_nbs += 1
        }
        else
          r = s.createRow(row_nbs)
        row_nbs += 1
        c = r.createCell(0)
        c.setCellValue(DAYS((fday + i - 1) % DAYS.size) + "  " + mtable.padnumber(i))
        c.setCellStyle(cs_title)
        for (idx <- 0 until tsize) {
          val day = i
          val state = mtable.getState(day,idx)
          val optcell = mtable.getCell(day,idx)
          c = r.createCell(idx+1)
          state match {
            case "Registered" | "Full" => {
              val cell = optcell.get 
              c.setCellValue(reg_map(day, cell.t, cell.s))
              c.setCellStyle(cs_reg)
            }
            case "Locked" => c.setCellStyle(cs_lock)
            case "Priority" => c.setCellStyle(cs_prio)
            case "Clear" =>
          }
        }
      }

      // footer
      r = s.createRow(row_nbs+1)
      c = r.createCell(0)
      c.setCellValue("Imprimé le:")
      c = r.createCell(1)
      c.setCellFormula("today()")

      // auto-sizing
      for (i <- 0 to subtitles.size)
        s.autoSizeColumn(i)

      wb.setSheetName(0, "planning")

      wb.write(out);
      out.close();

      val enum = Enumerator.fromStream(new ByteArrayInputStream(out.toByteArray))
      val name = "planning-"+mtable.m+"-"+mtable.y+".xls"

      SimpleResult(
        header = ResponseHeader(OK, Map(
                  //CONTENT_LENGTH -> content.length.toString,
                  CONTENT_TYPE -> play.api.libs.MimeTypes.forFileName(name).getOrElse(play.api.http.ContentTypes.BINARY),
                  CONTENT_DISPOSITION -> ("""attachment; filename="%s""".format(name)))
                  ),
        body = enum 
      )
    }
  }

  def submit_freetext() = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("t")("y","m"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val y = query("y")(0).toInt
      val m = query("m")(0).toInt
      val t = query("t")(0)

      transaction(Atomic()){ implicit conn =>
        try {
          SQL(
            """
              delete from freetext
              where year={y}
               and month={m}
            """
          ).on(
            "y" -> y,
            "m" -> m
          ).execute()

          SQL(
            """
              insert into freetext (year, month, data)
              values ({y}, {m}, {t})
            """
          ).on(
            "y" -> y,
            "m" -> m,
            "t" -> t
          ).execute()

          conn.commit
          Ok("ok")
        }
        catch { 
          case _ => {
            conn.rollback
            conn.commit
 
            Ok("er")
          }
        }
      }(Ok("er"))
    }
  }

  def submit_delete = Action { implicit request =>
    if(!isAdmin(session))
      Ok("erCette action est réservée à l'administrateur.")
    else if(!checkRequest("username")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val username = query("username")(0)
  
      if(username == "")
        Ok("ok")
      else if(username == "admin")
        Ok("erLe compte administrateur ne peut pas être supprimé!")
      else {
        transaction(Atomic()){ implicit conn =>
          try {
            /*WARNING : The order is CRUCIAL ! we cannot remove the username from users 
            before registrations or it violate the foreign key constraint */
            SQL(
              """
                delete from registrations
                where username={u}
              """
            ).on(
              "u" -> username
            ).execute()

            SQL(
              """
                delete from admins
                where username={u}
              """
            ).on(
              "u" -> username
            ).execute()

 
            SQL(
              """
                delete from users
                where username={u}
              """
            ).on(
              "u" -> username
            ).execute()
  
            conn.commit

            regenAllUsers()
            Ok("okPlus de trace de l'utilisateur '" + username + "'.")
          }
          catch {
            case _ => {
              conn.rollback
              conn.commit

              Ok("erContactez l'Administrateur (erreur: user_del).")
            }
          }
        }(Ok("er"))
      }
    }
  }
 
  def submit_add_admin = Action { implicit request => 
    if(!isAdmin(session))
      Ok("erCette action est réservée à l'administrateur.")
    else if(!checkRequest("username")())
      BadRequest(INVALARG)
    else {
      transaction(Committed()){ implicit conn =>
        try {
          val query = request.body.asFormUrlEncoded.get
          val username = query("username")(0)

          SQL(
            """
              insert into admins (username)
              values ({u})
            """
          ).on(
            "u" -> username
          ).execute()
          
          regenAllUsers()
          Ok("okL'utilisateur '" + username + "' a les droits administrateur.")
        }
        catch {
          case _ => Ok("erContactez l'Administrateur (erreur: admin_add).")
        }
      }(Ok("er"))
    }
  }

  //
  // Used in a POST when the administrator wants to remove
  // administrative rights to a given user.
  //
  def submit_remove_admin = Action { implicit request => 
    if(!isAdmin(session))
      Ok("erCette action est réservée à l'administrateur.")
    else if(!checkRequest("username")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val username = query("username")(0)
      
      transaction(Committed()){ implicit conn =>
        try {
          if(SQL(
            """
              delete from admins
              where username={u}
            """
          ).on(
            "u" -> username
          ).execute()){
            regenAllUsers()
            Ok("okL'utilisateur '" + username + "' n'a plus les droits administrateur.")
          }
          else
            Ok("ok")
        }
        catch {
          case _ => Ok("erContactez l'Administrateur (erreur: admin_del).")
        }
      }(Ok("er"))  
    }
  }

  //
  // Used in a POST when the administrator wants to signup
  // a new used with the given username and name.
  // 
  def submit_signup = Action { implicit request =>
    if(!isAdmin(session))
      Ok("erCette action est réservée à l'administrateur.")
    else if(!checkRequest("name","username","isadmin")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val name = query("name")(0)
      val username = query("username")(0)
      val isadmin = query("isadmin")(0) == "true"
      
      val err = (isName(name), isUsername(username)) match {
        case("","") => ""
        case("", err) => err
        case(err,"") => err
        case(err1,err2) => err1 + "<br \\><label for='hack'></label>" + err2
      }
      
      if(err != "")
        Ok("er" + err)
      else {
        transaction(Atomic()){ implicit conn =>
          try {
            val count = SQL(
              """
                select count(*) from users
                where name={n}
              """
            ).on("n" -> name).as(scalar[Long] single)

            if(count > 0) {
              conn.rollback
              conn.commit
 
              Ok("erCe nom est déjà utilisé.")
            } else {
              SQL(
                """
                  insert into users (username, name, password, salt) 
                  values ({u}, {n}, '', '')
                """
              ).on("u" -> username,
                   "n" -> name).execute()
 
              if(isadmin) {
                SQL(
                  """
                    insert into admins (username) 
                    values ({u})
                  """
                ).on("u" -> username).execute()
              }
              
              conn.commit

              regenAllUsers()

              if(isadmin)
                Ok("okL'utilisateur '" + username + "' a bien été enregistré, avec les droits administrateur.")
              else
                Ok("okL'utilisateur '" + username + "' a bien été enregistré.")
            }
          }
          catch {
            case _ => {
              conn.rollback
              conn.commit

              Ok("erLe nom d'utilisateur existe déjà.")
            }
          }
        }(Ok("er"))
      }
    }
  }
 
  // 
  // Used in a POST when the administrator wants to reset the
  // password of a given user. After that, the user will have
  // to do the same as when he first logged in: submit his password
  // in the 'login' page.
  // 
  def submit_reset_password = Action { implicit request =>
    if(!isAdmin(session))
      Ok("erCette action est réservée à l'administrateur.")
    else if(!checkRequest("username")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val username = query("username")(0)
  
      if(username == "")
        Ok("ok")
      else if(username == "admin")
        Ok("erLe mot de passe de l'administrateur ne peut pas être réinitialisé.")
      else {
        transaction(Committed()){ implicit conn =>
          try {
            SQL(
              """
                update users
                set password=''
                where username={u}
              """
            ).on(
              "u" -> username
            ).execute()
  
            Ok("okLe mot de passe de l'utilisateur '" + username + "' a bien été réinitialisé.")
          }
          catch {
            case _ => Ok("erContactez l'Administrateur (erreur: pass_res).")
          }
        }(Ok("er"))
      }
    }
  }

  //
  // Used in a POST when the administrator wants to submit the
  // manually changed registered users of a given month.
  //
  def submit_updateregistrations = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("t","s","u")("y","m","d"))
      BadRequest(INVALARG)
    else {
      val query    = request.body.asFormUrlEncoded.get
      val year     = query("y")(0).toInt
      val month    = query("m")(0).toInt
      val day      = query("d")(0).toInt
      val title    = query("t")(0)
      val subtitle = query("s")(0)
      val new_user = query("u")(0)

      transaction(Atomic()){ implicit conn =>
        try {
          SQL(
            """
              delete from registrations
              where year={y} and
                   month={m} and
                     day={d} and
                   title={t} and
                subtitle={s}
            """
          ).on(
            "y" -> year,
            "m" -> month,
            "d" -> day,
            "t" -> title,
            "s" -> subtitle
          ).execute()

          if(userExists(new_user)) {
            SQL(
              """
                insert into registrations (year, month, day, title, subtitle, username)
                values ({y}, {m}, {d}, {t}, {s}, {u})
              """
            ).on(
              "y" -> year,
              "m" -> month,
              "d" -> day,
              "t" -> title,
              "s" -> subtitle,
              "u" -> new_user
            ).execute()
          }

          conn.commit

          Ok("ok")
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit

            Ok("erContactez l'Administrateur (erreur: up_reg).")
          }
        }
      }(Ok("er"))
    }
  }

  def submit_priority() = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("m", "y"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get

      val add_indexes = req2listint(query.get("a[]"))
      val del_indexes = req2listint(query.get("d[]"))

      val tList = titles.toList
      val subtList = tList.foldLeft(Nil:List[String])(_ ::: _._2)
      val tsize = (titles.size, tList.map(_._2.size).sum)
      val month = query("m")(0).toInt
      val year = query("y")(0).toInt
      val ctab = new Table(month, year)

      def extract_local_data(i: Int): (Int,String,String,Int) = {
        val subidx = i % tsize._2
        val subtitle = subtList(subidx)
        val title = tList.find(_._2.contains(subtitle)).get._1 
        val day = 1 + i / subtList.size - ctab.prevDays.size
        (subidx,subtitle,title,day) 
      }
         
      transaction(Atomic()) { implicit conn =>
        try {
          for(i <- del_indexes) {
            val (subidx,subtitle,title,day) = extract_local_data(i) 
            
            SQL(
              """
                delete from priority
                where year={y} and
                     month={m} and
                       day={d} and
                     title={t} and
                  subtitle={s}
              """
            ).on(
              "y" -> year,
              "m" -> month,
              "d" -> day,
              "t" -> title,
              "s" -> subtitle
            ).execute()
          }

          for(i <- add_indexes) {
            val (subidx,subtitle,title,day) = extract_local_data(i) 
         
            SQL(
              """
                insert into priority (year, month, day, title, subtitle)
                values ({y}, {m}, {d}, {t}, {s})
              """
            ).on(
              "y" -> year,
              "m" -> month,
              "d" -> day,
              "t" -> title,
              "s" -> subtitle
            ).execute()
          }

          conn.commit

          Ok("")
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit

            Ok("Contactez l'Administrateur (erreur: prio_sub).")
          }
        }
      } (Ok("er"))
    }
  }

  //
  // Used in a POST when the administrator wants to submit all
  // cells users are not allowed to be registered, even if the whole
  // month is unlocked. This cannot be done on cells which already 
  // have registrations. This action can be done even if the whole
  // month is still locked.
  //
  def submit_lock() = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("m","y"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get

      val tList = titles.toList
      val subtList = tList.foldLeft(Nil:List[String])(_ ::: _._2)
      val tsize = (titles.size, tList.map(_._2.size).sum)
      val month = query("m")(0).toInt
      val year = query("y")(0).toInt
      val ctab = new Table(month, year) 
         
      val add_indexes = req2listint(query.get("a[]"))
      val del_indexes = req2listint(query.get("d[]"))

      def extract_local_data(i: Int): (Int,String,String,Int) = {
        val subidx = i % tsize._2
        val subtitle = subtList(subidx)
        val title = tList.find(_._2.contains(subtitle)).get._1 
        val day = 1 + i / subtList.size - ctab.prevDays.size
        (subidx,subtitle,title,day) 
      }
  
      transaction(Atomic()){ implicit conn =>
        try {
          for(i <- del_indexes) {
            val (subidx,subtitle,title,day) = extract_local_data(i)
            
            SQL(
              """
                delete from locked
                where year={y} and
                     month={m} and
                       day={d} and
                     title={t} and
                  subtitle={s}
              """
            ).on(
              "y" -> year,
              "m" -> month,
              "d" -> day,
              "t" -> title,
              "s" -> subtitle
            ).execute()
          }

          for(i <- add_indexes) {
            val (subidx,subtitle,title,day) = extract_local_data(i)
         
            SQL(
              """
                insert into locked (year, month, day, title, subtitle)
                values ({y}, {m}, {d}, {t}, {s})
              """
            ).on(
              "y" -> year,
              "m" -> month,
              "d" -> day,
              "t" -> title,
              "s" -> subtitle
            ).execute()
          }

          conn.commit

          Ok("")
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit

            Ok("Contactez l'Administrateur (erreur: lock_sub).")
          }
        }
      }(Ok("er"))
    }
  }

  //
  // The administrator has to manually unlock each month
  // he wants to let the users register, since they
  // are locked by default.
  //
  def submit_unlock_month() = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("y","m"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val year  = query("y")(0).toInt
      val month = query("m")(0).toInt

      transaction(Committed()){ implicit conn =>
        try {
          // We *know* that (year, month) are the
          // primary keys, so there *cannot* be
          // and double values.
          SQL(
            """
              insert into monthunlocked (year, month) 
              values ({y}, {m})
            """
          ).on("y" -> year,
               "m" -> month).execute()
          
          Ok("ok")
        }
        catch {
          case _ => Ok("erContactez l'Administrateur (erreur: month_lock).")
        }
      }(Ok("er"))
    }
  }

  //
  // The administrator may want to relock a month he has unlocked.
  // Note that this is only allowed if nobody registered in the mean
  // time.
  //
  def submit_relock_month() = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("y","m"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val year  = query("y")(0).toInt
      val month = query("m")(0).toInt
      
      if(isLockable(year, month)) {
        var haserror = false
        
        transaction(Committed()){ implicit conn =>
          try {
            SQL(
              """
                delete from monthunlocked
                where year={y} and
                     month={m}
              """
            ).on("y" -> year,
                 "m" -> month).execute()
          } catch {
            case _ =>
          }
        }()
      }
      
      Redirect("/lock_view"+"?m="+month+"&y="+year)
    }
  }

  //
  // Used in a POST when the administrator wants to
  // add a title.
  //
  def submit_add_title = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("title")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val title = sanatize_input(query("title")(0))
  
      if(isempty(title))
        Ok("error")
      else {
        transaction(Committed()){ implicit conn =>
          try {
            SQL(
              """
                insert into titles (title)
                values ({t})
              """
            ).on(
              "t" -> title
            ).execute()

            regenTitles()
            Ok("")
          }
          catch {
            case _ => Ok("error")
          }
        }(Ok("error"))
      }
    }
  }

  //
  // Used in a POST when the administrator wants to
  // add a subtitle linked to a title. A subtitle does
  // not have to be unique across titles.
  //
  def submit_add_subtitle = Action { implicit request => 
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("title","subtitle")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      var ret = Ok("")
  
      val r = """^[1-9]\d*$""".r

      val nb = get_next_order()
  
      val title = sanatize_input(query("title")(0))
      val subtitle = sanatize_input(query("subtitle")(0))

      if(isempty(title) || isempty(subtitle))
        Ok("error")
      else {
        transaction(Committed()){ implicit conn =>
          try {
            SQL(
              """
                insert into activities (title, subtitle, "order")
                values ({t}, {s}, {n})
              """
            ).on(
              "t" -> title,
              "s" -> subtitle,
              "n" -> nb
            ).execute()

            regenTitles()
          }
          catch {
            case _ => ret = Ok("error")
          }
        }()
      }
      
      ret
    }
  }

  //
  // Used in a POST when the administrator wants to delete
  // a given title.
  //
  def submit_delete_title = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("title")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val title = sanatize_input(query("title")(0))
  
      transaction(Atomic()){ implicit conn =>
        try {
          SQL(
            """
              delete from activities
              where title={t}
            """
          ).on(
            "t" -> title
          ).execute()
  
          SQL(
            """
              delete from titles
              where title={t}
            """
          ).on(
            "t" -> title
          ).execute()
       
          //FOR STATISTICS
          /*SQL(
            """
              delete from registrations
              where title={t}
            """
          ).on(
            "t" -> title
          ).execute()*/
    
          SQL(
            """
              delete from locked
              where title={t}
            """
          ).on(
            "t" -> title
          ).execute()

          SQL(
            """
              delete from priority 
              where title={t}
            """
          ).on(
            "t" -> title
          ).execute()
 
          conn.commit
          
          regenTitles()

          Ok("")
        }
        catch {
          case _ => {
            conn.rollback
            conn.commit
            
            Ok("error")
          }
        }
      }(Ok("error"))
    }
  }

  //
  // Used in a POST when the administrator wants to
  // update the titles arrangement/modifications 
  //
  def submit_update_titles = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest()("nbt"))
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val nbt = sanatize_input(query("nbt")(0)).toInt
      val checks = for (i <- 0 until nbt) yield {
        checkRequest("titles["+i+"][t]","titles["+i+"][s][]")()
      }
      if(!checks.fold(true)(_ && _))
        BadRequest(INVALARG)
      else {
        val newTitles = (for (i <- 0 until nbt) yield {
          val tidx: Int = query("titles["+i+"][t]")(0).toInt
          val subidx: List[Int] = query("titles["+i+"][s][]").toList.map(_.toInt)
          (titles(tidx)._1, subidx.map(sidx => titles(tidx)._2(sidx)))
        }).toList

        transaction(Committed()){ implicit conn =>
          try {
            var ord = 0
            for (titles <- newTitles) {
              for (subtitle <- titles._2) {
                SQL(
                  """
                    update activities 
                    set "order"={o}
                    where title={t} and
                       subtitle={s}
                  """
                ).on(
                  "o" -> ord,
                  "t" -> titles._1,
                  "s" -> subtitle
                ).execute()

                ord += 1
              }
            }

            regenTitles()

            Ok("")
          }
          catch {
            case _ => Ok("erContactez l'Administrateur (erreur: up_order).")
          }
        }(Ok("er"))  
      }
    }
  }

  //
  // Used as a POST when the administrator wants to delete a 
  // given subtitle linked to a given activity.
  // Since subtitles can be the same for two different titles,
  // this argument is needed.
  //
  def submit_delete_subtitle = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else if(!checkRequest("title","subtitle")())
      BadRequest(INVALARG)
    else {
      val query = request.body.asFormUrlEncoded.get
      val title = sanatize_input(query("title")(0))
      val subtitle = sanatize_input(query("subtitle")(0))
  
      transaction(Atomic()){ implicit conn =>
        try {
          SQL(
            """
              delete from activities
              where title={t} and
                 subtitle={s}
            """
          ).on(
            "t" -> title,
            "s" -> subtitle
          ).execute()

          //FOR STATISTICS
          /*SQL(
            """
              delete from registrations
              where title={t} and
                 subtitle={s}
            """
          ).on(
            "t" -> title,
            "s" -> subtitle
          ).execute()*/
          
          SQL(
            """
              delete from locked
              where subtitle={s}
                   and title={t}
            """
          ).on(
            "t" -> title,
            "s" -> subtitle
          ).execute()

          SQL(
            """
              delete from priority 
              where subtitle={s}
                   and title={t}
            """
          ).on(
            "t" -> title,
            "s" -> subtitle
          ).execute()

          conn.commit 

          regenTitles()

          Ok("")
        }
        catch {
          case e => {
            conn.rollback
            conn.commit

            Ok("error")
          }
        }
      }(Ok("error"))
    }
  }

}
