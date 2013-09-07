
 * Default administrator login:

   username: admin
   password: admin-rive

 * Default Guest login:
  
   username: guest
   password: guest-rive

 * Jquery / UI

   - Jquery version 2.0.3
   - Jquery-UI version 1.10.3

 * Bootstrap
   
   - Bootstrap version 2.3.2

 * Security

   - Passwords are store on the server salted.
     - store as : H(password | salt) with a random 256 bit salt
   - Password requirement for users is :
     - at least 7 characters
     - at least one lower case / upper case / number
