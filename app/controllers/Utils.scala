package controllers

import play.api.mvc.{Controller, Session, Request, AnyContent}
import play.api.db.DB // db
import play.api.Play.current
import play.api.templates.Html
import anorm.{SQL, SimpleSql, ~} // sql
import anorm.SqlParser.{str, int, scalar, flatten} //sql parser
import java.util.{Calendar, GregorianCalendar}
import java.sql.Connection
import scala.util.Random
import scala.collection.mutable.HashMap
import com.roundeights.hasher.Implicits.stringToHasher // string to sha256

object Utils extends Controller {

  /***************
  * TRANSACTIONS *
  ****************/

  abstract class TransactionType
  case class Atomic() extends TransactionType
  case class Committed() extends TransactionType

  def transaction[T](ttype: TransactionType)(block: (Connection) => T)(default: T) = {
    try {
      DB.withConnection { implicit conn =>
        ttype match {
          case Atomic() => conn.setAutoCommit(false)
          case Committed() => //autocmmit is true by default : conn.setAutoCommit(true)
        }

        block(conn)
      }
    }
    catch {
      case _ : Throwable => default
    }
  }

  def cleanHtml(html: Html): Html = {
    val filtStartEmpty = """^(\s*\n)+""".r.replaceAllIn(html.body,"")
    val filtEmpty = """\n(\s*\n)+""".r.replaceAllIn(filtStartEmpty,"\n")
    val filtComma = """,\s*\n\s*""".r.replaceAllIn(filtEmpty,",")
    Html(filtComma)
  }

  def get_registered_users_map(month: Int, year: Int): HashMap[(Int,String,String),String] = {
    transaction(Committed()){ implicit conn =>
      try {
        val u_reg = SQL(
          """
            select day,title,subtitle,name
            from registrations,users
            where registrations.username=users.username and
                  month={m} and
                   year={y}
          """
        ).on("m" -> month,
             "y" -> year).as(int("day") ~ str("title") ~ str("subtitle") ~ str("name") map({case d~t~s~n => ((d,t,s),n)}) *)

        HashMap(u_reg:_*)
      }
      catch {
        case _ => HashMap.empty[(Int,String,String),String]
      }
    }(HashMap.empty[(Int,String,String),String])
  }

  var allUsers: List[String] = genAllUsers
  var allUsersString: String = genAllUsersString
  var adminUsers: List[String] = genAdminUsers
  var normalUsers: List[String] = genNormalUsers

  def regenAllUsers(): Unit = {
    genAllUsers
    genAllUsersString
    genAdminUsers
    genNormalUsers
  }

  // A list of all users, excluding 'admin'.
  def genAllUsers(): List[String] = {
    transaction(Committed()){ implicit conn =>
      try {
        val userlist = SQL(
          """
            select username from users
            where username != 'admin'
          """
        ).as(str("username") *).sorted

        allUsers = userlist
        userlist
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  // generate the pair (username,name) sorted by username
  def genAllNames(): List[(String,String)] = {
    transaction(Committed()){ implicit conn =>
      try {
        val names = SQL(
          """
            select username,name from users
            where username != 'admin'
          """
        ).as(str("username") ~ str("name") map(flatten) *)

        names.sortWith(_._1 < _._1)
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  // table of table in Javascript format. : [[username, name], ...]
  def genAllUsersString(): String = {
    val ret = "[" + genAllNames.map(x => "[\"" + x._1 + "\",\"" + x._2 + "\"]").mkString(",") + "]"
    allUsersString = ret
    ret
  }

  // Gets all users with administrative rights.
  def genAdminUsers(): List[String] = {
    transaction(Committed()){ implicit conn =>
      try {
        val adminlist = SQL(
          """
            select username from admins
          """
        ).as(str("username") *).sorted

        adminUsers = adminlist
        adminlist
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  // The opposite of the above.
  def genNormalUsers(): List[String] = {
    transaction(Committed()){ implicit conn =>
      try {
        val nuserlist = SQL(
          """
            select username from users
            where not exists (
              select username from admins
              where users.username = admins.username
            )
          """
        ).as(str("username") *).filter(_ != "admin").sorted

        normalUsers = nuserlist
        nuserlist
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  // Tests the existance of a given user.
  def userExists(username: String): Boolean = {
    transaction(Committed()){ implicit conn =>
      try {
        val count = SQL(
          """
            select count(username) from users
            where username={u}
          """
        ).on(
          "u" -> username
        ).as(scalar[Long] single)
        count != 0
      }
      catch {
        case _ => false
      }
    }(false)
  }

  def getFreeText(month: Int, year: Int): String = {
    transaction(Committed()){ implicit conn =>
      try {
        SQL(
          """
            select data from freetext
            where year={y}
             and month={m}
          """
        ).on(
          "y" -> year,
          "m" -> month
        ).as(str("data") singleOpt).getOrElse("")
      }
      catch {
        case _ => ""
      }
    }("")
  }

  def get_mail(username: String): String = {
    transaction(Committed()){ implicit conn =>
      try {
        SQL(
          """
            select mail from mails
            where username={u}
          """
        ).on(
          "u" -> username
        ).as(str("mail") singleOpt).getOrElse("")
      }
      catch {
        case _ => ""
      }
    }("")
  }

  /************************
  * A few constant values *
  *************************/
  val DAYS = List("Lu", "Ma", "Me", "Je", "Ve", "Sa", "Di")
  val MONTHS = List(1 -> "Janvier", 2 -> "Février", 3 -> "Mars",       4 -> "Avril",    5 -> "Mai",       6 -> "Juin",
                    7 -> "Juillet", 8 -> "Août",    9 -> "Septembre", 10 -> "Octobre", 11 -> "Novembre", 12 -> "Décembre")
  val ERRLOGIN = "erNom d'utilisateur ou mot de passe invalide."
  val ERRFIRST = "erPas besoin de définir de mot de passe pour cet utilisateur."
  val INVALARG = "erInvalid arguments"

  def date(key: String): Int = {
    val cal = Calendar.getInstance()
    cal.setFirstDayOfWeek(Calendar.MONDAY)

    key match {
      case "d" => cal.get(Calendar.DAY_OF_MONTH)
      case "m" => cal.get(Calendar.MONTH) + 1
      case "y" => cal.get(Calendar.YEAR)
      case "h" => cal.get(Calendar.HOUR_OF_DAY)
      case "min" => cal.get(Calendar.MINUTE)
      case "sec" => cal.get(Calendar.SECOND)
    }
  }

  def resolve_date(month: Int, year: Int): (Int,Int) = {
    val m = if(month <= 0 || month > 12) date("m") else month
    val y = if(year < 2013 || year > 2100) date("y") else year
    (m,y)
  }

  var titles: List[(String, List[String])] = Nil
  var emptyTitles: List[String] = Nil

  // first generation (after re-compilation)
  regenTitles()

  def regenTitles() = {
    titles = genTitles
    emptyTitles = genEmptyTitles
  }

  // Returns the list of titles to be shown on the top of the calendar.
  def genTitles(): List[(String, List[String])] = {
    transaction(Committed()){ implicit conn =>
      try {
        val tuplist = SQL(
          """
            select title, subtitle, "order"
            from activities
            order by "order"
          """
        ).as(str("title") ~ str("subtitle") map(flatten) *)

        // TYPE : List[(String, List[String])]
        val title = tuplist.map(_._1).distinct.map(y => (y,tuplist.filter(_._1 == y).map(_._2)))

        title
      }
      catch {
        case _ => Nil: List[(String, List[String])]
      }
    }(Nil: List[(String, List[String])])
  }

  // return the next free order index
  def get_next_order(): Int = {
    transaction(Committed()){ implicit conn =>
    try {
      1 + SQL(
       """
        select max("order")
        from activities
       """
      ).as(scalar[Int] single)
    }
    catch {
      case _ => 0
    }
    }(0)
  }

  // In category, it is possible to create titles without
  // a subtitle. If no subtitle is attached, it will not
  // be shown in the calendar. The empty titles
  // are still shown in the category page by using this
  // function.
  def genEmptyTitles(): List[String] = {
    transaction(Committed()){ implicit conn =>
      try {
        SQL(
          """
            select title from titles
            where not exists (
              select title from activities
              where titles.title = activities.title
            )
          """
        ).as(str("title") *)
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  /**************
  * CELL STATES *
  ***************/
  abstract class CellState
  case class AlreadyReg(name: String) extends CellState
  case class Full(name: String) extends CellState
  case class Locked_() extends CellState
  case class Priority() extends CellState

  /*************
  * CELL CLASS *
  **************/
  class Cell(state: CellState, tup: (Int, String, String)) {
    val d = tup._1 //day
    val t = tup._2 //title
    val s = tup._3 //subtitle
    val st = state

    def get_name: String = state match {
        case AlreadyReg(name) => name
        case Full(name) => name
        case _ => ""
    }

    def state_str: String = state match {
        case Locked_() => "Locked"
        case AlreadyReg(_) => "Registered"
        case Full(_) => "Full"
        case Priority() => "Priority"
    }
  }

  /****************
  * TABLE OF CELL *
  *****************/
  class Table(month: Int, year: Int, locked: Boolean = true, free_text: String = "") {
    var cells: List[Cell] = Nil
    val m: Int = month
    val y: Int = year
    val is_locked: Boolean = locked
    val freetext: String = free_text

    val gcal = new GregorianCalendar(year, month - 1,1)

    def addCell(cell: Cell) {
      cells = cells :+ cell
    }

    // Returns the number of days in the given month and year.
    val nbrOfDays: Int = {
      gcal.getActualMaximum(Calendar.DAY_OF_MONTH)
    }

    // between 0 -> 6
    val firstDayOfMonth: Int = {
      (gcal.get(Calendar.DAY_OF_WEEK) - 2 + 7) % 7
    }

    val weekOfYear: Int = {
      gcal.get(Calendar.WEEK_OF_YEAR)
    }

    // List of the days from last month appearing in the current month.
    val prevDays: List[Int] = {
      val fday = firstDayOfMonth
      val prevCal = new GregorianCalendar(year, month - 2,1)
      val maxday = prevCal.getActualMaximum(Calendar.DAY_OF_MONTH)
      List.tabulate(fday)(_ +  maxday - fday + 1)
    }

    // Returns the number of weeks in a given month and year.
    // This result is always between 4 and 6.
    val nbrOfWeeks: Int = {
      math.ceil((prevDays.size + nbrOfDays) / 7.0).toInt
    }

    val padSize: Int = {
      (7 * nbrOfWeeks) - (nbrOfDays + prevDays.size)
    }

    // Returns all days to be shown on the page; the days before the month,
    // the actual month, and the days after the month in the same week.
    val allDays: List[List[Int]] = {
      val rest: Int = padSize
      val days: List[Int] = prevDays :::
                            (1 to nbrOfDays).toList :::
                            (1 to rest).toList
      var alld: List[List[Int]] = Nil
      for(i <- 0 until nbrOfWeeks) {
        alld = alld :+ days.slice(7 * i,7 * (i+1))
      }
      alld
    }

    val allDaysMonth: List[List[String]] = {
      val rest: Int = padSize
      val prev_m: Int = if(m == 1) 12 else m - 1
      val next_m: Int = if(m == 12) 1 else m + 1
      val days: List[String] = (prevDays map (x => padnumber(x) + "." + padnumber(prev_m))) :::
                               (1 to nbrOfDays).toList.map(x => padnumber(x) + "." + padnumber(m)) :::
                               (1 to rest).toList.map(x => padnumber(x) + "." + padnumber(next_m))
      var alld: List[List[String]] = Nil
      for(i <- 0 until nbrOfWeeks) {
        alld = alld :+ days.slice(7 * i,7 * (i+1))
      }
      alld
    }

    def padnumber(n: Int): String = {
      if(n < 10) "0" + n
      else "" + n
    }

    // Test if the given day is out of the current month.
    def isOut(week: Int, day: Int): Boolean = {
      (week == 0 && day < prevDays.size || (week == nbrOfWeeks - 1) && day >= 7 - padSize)
    }

    def getTitles(day: Int, tidx: Int): (String, String) = {
      var idx = tidx
      var tsub = ("","")
      for(tup <- titles) {
        if(idx >= 0 && idx < tup._2.size) {
          tsub = (tup._1, tup._2(idx))
          idx = -1
        } else
          idx = idx - tup._2.size
      }
      tsub
    }

    def getCell(day: Int, tidx: Int): Option[Cell] = {
      val tsub = getTitles(day, tidx)
      cells.find(x => x.d == day && x.t == tsub._1 && x.s == tsub._2)
    }

    // Returns a state string used in Javascript.
    def getState(day: Int, tidx: Int): String = {
      getCell(day, tidx) match {
        case Some(cell) => cell.state_str
        case None => "Clear"
      }
    }

    // Test if given date is in the past according
    // to the internal clock.
    def isInPast(day: Int): Boolean = {
      y < date("y") ||
      y == date("y") && m  < date("m") ||
      y == date("y") && m == date("m") && day < date("d")
    }
  }

  /*****************
  * LOGGING STATES *
  ******************/
  abstract class LogState
  case class Admin(name: String) extends LogState  // Logged with administrator rights.
  case class User(name: String) extends LogState   // Logged as user
  case class Guest() extends LogState              // Logged as guest
  case class NotLog() extends LogState             // Not logged

  def get_username(lg: LogState): String = lg match {
    case Admin(name) => name
    case User(name) => name
    case _ => ""
  }

  /*****************
  * CRAZY IMPLICIT *
  ******************/

  // With a cookie, determines if we are logged and with which rights.
  implicit def getStat(session: Session): LogState = {
    session.get("username") match {
      case Some(name) => userToState(name)
      case None => NotLog()
    }
  }

  // Are we admin?
  private def userToState(name: String): LogState = {
    if(name == "admin")
      Admin("admin")
    else if(name == "guest")
      Guest()
    else if(!userExists(name))
      NotLog()
    else {
      transaction(Committed()){ implicit conn =>
        try {
          val count = SQL(
            """
              select count(*) from admins
              where username={u}
            """
          ).on(
            "u" -> name
          ).as(scalar[Long] single)

          if(count == 0)
            User(name)
          else
            Admin(name)
        }
        catch {
          case _ => NotLog()
        }
      }(NotLog())
    }
  }

  /***********
  * SECURITY *
  ************/
  def isAdmin(session: Session) = getStat(session) match {
    case Admin(_) => true
    case _ => false
  }

  def isLogged(session: Session) = getStat(session) match {
    case Admin(_) | User(_) => true
    case _ => false
  }

  val GUEST_USR = "guest"
  val GUEST_PASS = "guest-rive"

  def isGuestLog(query: Map[String,Seq[String]]): Boolean = {
    val username = query("username")(0)
    val pass = query("password")(0)

    username == GUEST_USR && pass == GUEST_PASS
  }

  def isGuest(session: Session) = getStat(session) match {
    case Guest() => true
    case _ => false
  }

  def randHash(): String = {
    new Random().nextString(32).sha256.toString
  }

  // check that all the 'arg' argument exists and than all the 'num' argument are numbers
  def checkRequest(arg: String*)(nums: String*)(implicit request: Request[AnyContent]): Boolean = {
    val args = arg ++ nums
    request.body.asFormUrlEncoded match {
      case Some(query) => args.forall(x => query.contains(x)) &&
                          args.forall(x => query(x) != Nil) &&
                          nums.forall(x => isNumber(query(x)(0)))
      case None => false
    }
  }

  // case (if possible) a tab request into a List[Int] or Nil
  def req2listint(tab: Option[Seq[String]]): List[Int] = tab match {
    case Some(lis) if lis forall isNumber => lis map(_.toInt) toList
    case _ => Nil
  }

 /**************
 * ADMIN PAGES *
 ***************/

  def hasNewRegistrations: (Boolean, String) = {
    transaction(Committed()){ implicit conn =>
      try {
        val res = SQL(
            """
              select * from params
            """
        ).as(str("param") ~ int("value") map(flatten) *).toMap

        val uy   = res("user_y")
        val um   = res("user_m")
        val ud   = res("user_d")
        val uh   = res("user_h")
        val umin = res("user_min")
        val usec = res("user_sec")
        val ay   = res("admin_y")
        val am   = res("admin_m")
        val ad   = res("admin_d")
        val ah   = res("admin_h")
        val amin = res("admin_min")
        val asec = res("admin_sec")

        def toStr: Int => String = {
          x => (if(x < 10) "0" else "") + x
        }

        (((uy >  ay
        || (uy == ay
         && (um >  am
         || (um == am
          && (ud >  ad
          || (ud == ad
           && (uh >  ah
           || (uh == ah
            && (umin >  amin
            || (umin == amin
             && (usec >  asec)))))))))))), toStr(ud) + "/" + toStr(um) + " " + toStr(uh) + ":" + toStr(umin))
      }
      catch {
        case _ => (false, "")
      }
    }((false, ""))
  }

/*********************
* MISCELANEOUS UTILS *
**********************/

  // Generate a table that contains everyone's registrations
  // in the given month and year. It contains everyone's name
  // and the state of the cell: Is it full? Is it empty? Are there
  // any places left?
  def genTable(month: Int, year: Int)(implicit request: Request[Any]): Table = {
    val mtable = new Table(month, year, !unlocked_month(month,year), getFreeText(month,year))

    transaction(Committed()){ implicit conn =>
      try {
        /* Get all data to a list. No element is nullable in this table. */
        val filtered = SQL(
          """
            select day, title, subtitle, registrations.username, name
            from registrations, users
            where registrations.username=users.username and
                  month={m} and
                   year={y}
          """
        ).on("y" -> year,
             "m" -> month).as(int("day") ~ str("title") ~ str("subtitle") ~ str("username") ~ str("name")
                              map{case d~t~s~u~n => ((d,t,s),u,n)} *)

        /* Split in two lists, one with our registration, and the other
         * with the others registration. */
        val (mine, others) = filtered.partition(x => getStat(session) match {
          case Admin(user) => x._2 == user
          case User(user)  => x._2 == user
          case _           => false
        })

        val locked = SQL(
          """
            select day, title, subtitle
            from locked
            where year={y} and
                 month={m}
          """
        ).on("y" -> year,
             "m" -> month).as(int("day") ~ str("title") ~ str("subtitle") map(flatten) *)

        val priority = SQL(
          """
            select day, title, subtitle
            from priority
            where year={y} and
                 month={m}
          """
        ).on("y" -> year,
             "m" -> month).as(int("day") ~ str("title") ~ str("subtitle") map(flatten) *)

        for(x <- mine)
          mtable.addCell(new Cell(AlreadyReg(x._3), x._1))

        for(x <- others)
          mtable.addCell(new Cell(Full(x._3), x._1))

        for(x <- locked)
          mtable.addCell(new Cell(Locked_(), x))

        // Take away all those which already have a cell.
        val priorityCells = priority.filterNot(x => mtable.cells.exists(y => y.d == x._1 && y.t == x._2 && y.s == x._3))

        for(x <- priorityCells)
          mtable.addCell(new Cell(Priority(), x))
      }
      catch {
        case _ =>
      }
    }()

    // Is this right?
    mtable
  }

  // Returns true if month in unlocked for users
  // to register.
  def unlocked_month(month: Int, year: Int): Boolean = {
    transaction(Committed()){ implicit conn =>
      try {
        val count = SQL(
          """
            select count(*) from monthunlocked
            where year={y} and
                 month={m}
          """
        ).on("y" -> year,
             "m" -> month).as(scalar[Long] single)

        /* Get all data to a list. No element is nullable in this table,
           and there is only one result. */
        count == 1
      }
      catch {
        case _ => false
      }
    }(false)
  }

  // Test if the given month can be re-locked by the administrator.
  // This can only be done if nobody registered in the month
  // since it was unlocked.
  def isLockable(month: Int, year: Int): Boolean = {
    transaction(Committed()){ implicit conn =>
      try {
        val count = SQL(
          """
            select count(*) from registrations
            where year={y} and
                 month={m}
          """
        ).on("y" -> year,
             "m" -> month).as(scalar[Long] single)

        /* Get all data to a list. No element is nullable in this table,
           and there is only one result. */
        count == 0
      }
      catch {
        case _ => false
      }
    }(false)
  }

  // We do not accept _, because spaces are already
  // replaced by _ when used in the Html file to post
  // changed. Like this, we do not permit names
  // Like 'foo bar' and 'foo_bar', which will be
  // shown the same.
  def sanatize_input(input: String): String = {
    val t = "_".r.replaceAllIn(input, s => " ")
    val q = """\s+$""".r.replaceAllIn(t, s => "")
    val r = """^\s+""".r.replaceAllIn(q, s => "")
    val p = """\s+""".r.replaceAllIn(r, s => " ")
    """['",]""".r.replaceAllIn(p, s => "")
  }

  /* * * * * * * * * * *
   * MINIMUM NAME SIZE *
   * * * * * * * * * * */
  val MIN = 3  // Only usernames and names of this range
  val MAX = 12 // are accepted.

  // Decide if the input is a valid username
  def isUsername(input: String): String = {
    if(input matches "^[a-z]{"+MIN+","+MAX+"}$")
      ""
    else if(input matches "^$")
      "Le nom d'utilisateur est vide"
    else if(input matches "^[a-z]{1,"+(MIN-1)+"}$")
      "Le nom d'utilisateur est trop court"
    else if(input matches "[a-z]{"+(MAX+1)+",}")
      "Le nom d'utilisateur est trop long"
    else if(input matches ".*[A-Z].*")
      "Le nom d'utilisateur ne peut pas contenir de majuscule"
    else if(input matches ".* .*")
      "Le nom d'utilisateur ne peut pas contenir d'espace"
    else
      "Le nom d'utilisateur contient des caractères invalides : " + input.replaceAll("""[a-z]""","").distinct.mkString(" ")
  }

  // Returns a nice readable error if the given shortered name
  // is not of the right format.
  def isName(input: String): String = {
    if(input matches "^$")
      "Le nom abrégé est vide"
    else if(input matches """.*['"<>].*""")
      "Le nom abrégé contient des caractères invalides : " + input.replaceAll("""[^'"<>]""","").distinct.mkString(" ")
    else if(input matches "^.{0,"+(MIN-1)+"}$")
      "Le nom abrégé est trop court"
    else if(input matches "^.{"+(MAX+1)+",}$")
      "Le nom abrégé est trop long"
    else
      ""
  }

  def isNumber(input: String): Boolean = {
    input matches """^[1-9]\d*$"""
  }

  // Tests if the string contains only spaces.
  def isempty(input: String): Boolean = {
    input matches """^\s*$"""
  }

  // TODO recheck this
  def is_mail(input: String): String = {
    val check = input matches """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"""
    if(check || input == "") "" else "l'adresse mail n'est pas valide"
  }

 /**************
  * STATISTICS *
  **************/

  //
  // Gets the number of times a user is registered in a given year.
  // Optionally, we can specify a specifig month in the year.
  //
  def getStatistics(year: Int, month: Option[Int]): List[(String, String, Int)] = {
    val optargs: (String, (SimpleSql[_]) => SimpleSql[_]) = month match {
      case Some(m) => (" and month={m}", ssql => ssql.on("m" -> m))
      case None => ("", ssql => ssql)
    }

    val sqlreq =
      """
        select username, title from registrations
        where year={y}
      """ + optargs._1

    transaction(Committed()){ implicit conn =>
      try {
        val req = SQL(sqlreq).on("y" -> year)
        val rawdata = optargs._2(req).as(str("username") ~ str("title") map(flatten) *)

        rawdata.distinct.map(x => (x._1, x._2, rawdata.count(_ == x)))
      }
      catch {
        case _ => Nil
      }
    }(Nil)
  }

  //
  // Returns the result of getStatistics in a nice map.
  //
  def getStatMap(year: Int, month: Option[Int]): HashMap[String, HashMap[String, Int]] = {
    def defaultInnerMap() = new HashMap[String, Int](){override def default(key: String) = 0}
    val map = new HashMap[String, HashMap[String, Int]](){override def default(key: String) = defaultInnerMap()}

    for((username, title, nbr) <- getStatistics(year, month)) {
      map.get(title) match {
        case Some(innermap) => innermap += (username -> nbr)
        case None => map += (title -> (defaultInnerMap() += (username -> nbr)))
      }
    }

    map
  }

}
