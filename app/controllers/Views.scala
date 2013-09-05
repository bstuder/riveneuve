package controllers

import play.api.mvc.{Controller, Action}
import play.api.templates.Html

import Utils.{date, resolve_date, genTable, titles, isLogged, isGuest, isLockable, isAdmin, cleanHtml}

object Views extends Controller {

 /***************
 * PUBLIC PAGES *
 ****************/

  // By default, redirect to the login page, since we
  // can't do anything if we are not logged.
  def index = Action { implicit request =>
    Redirect("/login_view")
  }

  // If we are already logged, directly go to calendar.
  def login_view = Action { implicit request =>
    if(!isLogged(session) && !isGuest(session))
      Ok(cleanHtml (views.html.login(session)))
    else
      Redirect("/calendar_view")
  }

 /**************
 * USERS PAGES *
 ***************/
  def calendar_view(month: Int, year: Int) = Action { implicit request =>
    if(!isLogged(session) && !isGuest(session))
      Forbidden(views.html.forbidden())
    else {
      val (m,y) = resolve_date(month,year)
      Ok(cleanHtml (views.html.calendar(session)(genTable(m,y), isLockable(m,y), titles)))
    }
  }

  def password_view = Action { implicit request =>
    if(!isLogged(session))
      Forbidden(views.html.forbidden())
    else
      Ok(cleanHtml (views.html.password(session)))
  }

 /**************
 * ADMIN PAGES *
 ***************/
 
  def statistics_view(month: Int, year: Int) = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else {
      val (m,y) = resolve_date(month,year)
      Ok(cleanHtml (views.html.statistics(session, m, y)))
    }
  }

  def category_view = Action { implicit request => 
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else
      Ok(cleanHtml (views.html.category(session, titles)))
  }
  
  def signup_view = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else
      Ok(cleanHtml (views.html.signup(session)))
  }

  //
  // The page the administrator looks at to choose if he wants
  // to lock/unlock months or cells.
  //
  def lock_view(month: Int, year: Int) = Action { implicit request => 
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else {
      val (m,y) = resolve_date(month,year)
      Ok(cleanHtml (views.html.calendar(session)(genTable(m,y), isLockable(m,y), titles, true)))
    }
  }

  def priority_view(month: Int, year: Int) = Action { implicit request =>
    if(!isAdmin(session))
      Forbidden(views.html.forbidden())
    else {
      val (m,y) = resolve_date(month,year)
      Ok(cleanHtml (views.html.calendar(session)(genTable(m,y), isLockable(m,y), titles, false, true)))
    }
  }

}
