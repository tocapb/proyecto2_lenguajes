--Entradas:Ninguna
--Salidas:Ninguna
--Funcionalidad:Menú inicial introduciendo el usuario que permite acceder a las funcionalidades
inicio:: IO()
inicio= do
    putStr("\nBienvenido\n")
    putStr("Introduzca el usuario:")
    usuario <- getLine
    case usuario of
        "admin" -> menu_principal
        
    return()
--Entradas:Ninguna
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las opciones operativos o generales
menu_principal:: IO()
menu_principal= do
    putStr("\nMenú Principal\n")
    putStr("1.Opciones Operativas\n")
    putStr("2.Opciones Generales \n")
    putStr("3.Salir \n")
    putStr("Introduzca su opción:")
    opcion <- getLine
    case opcion of
        "1" -> menu_operativo
        "2" -> menu_general
        "3" -> return()
        
--Entradas:Ninguna
--Salidas:Ninguna
--Funcionalidad:Menú operativo para acceder a las funciones operativas
menu_operativo:: IO()
menu_operativo= do
    putStr("\nMenú Operativo\n")
    putStr("1.Mostrar parqueos\n")
    putStr("2.Mostrar bicicletas \n")
    putStr("3.Mostrar usuarios \n")
    putStr("4.Estadísticas \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    opcion <- getLine
    case opcion of
        "1" -> putStr("1")
        "2" -> putStr("2")
        "3" -> putStr("3")
        "4" -> putStr("4")
        "5" -> menu_principal
    return()

--Entradas:Ninguna
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las funciones generales   
menu_general:: IO()
menu_general= do
    putStr("\nMenú Generales\n")
    putStr("1.Consultar bicicletas\n")
    putStr("2.Alquiler\n")
    putStr("3.Facturar \n")
    putStr("4.Consulta de factura \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    opcion <- getLine
    case opcion of
        "1" -> putStr("1")
        "2" -> putStr("2")
        "3" -> putStr("3")
        "4" -> putStr("4")
        "5" -> menu_principal
    return()