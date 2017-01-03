SECURITY NOTICE

I would recommend not to put the shared folder as a subfolder of the shiny-server directory. If you put it under the shiny-server directory and there is no shiny app inside or above the folder all files (e.g. the user data base) can be accessed from the webbrowser!

The global.R, ui.R and server.R in this folder, should prevent file access, but I would really recommend to put the share folder in a path outside the shiny-server folder.

You then have to adapt the main.dir in the global.R files in all apps to the path of the shared folder on your server.
