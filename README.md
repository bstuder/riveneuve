
 * Default administrator login:

   username: admin
   password: admin-rive

 * Default Guest login:
  
   username: guest
   password: guest-rive

 * Jquery / UI

   - Jquery version 2.0.3
   - Jquery-UI version 1.10.3

 * Security

   - Passwords are store on the server salted.
     - store as : H(password | salt) with a random 256 bit salt
   - Password requirement for users is :
     - at least 7 characters
     - at least one lower case / upper case / number


 ---------------------
 -- Hidden features --
 ---------------------

 It is possible to enable multiple persons per cell.

 However, in the requierments, the 'partially full' message
 seems too complicated for some people. Therefore, we fixed the
 number of registered users to 1, and multiple users are handles by
 having multiple colomns.

 To renable this feature, uncomment the code where 'HIDDEN FEATURE'
 can be found.
