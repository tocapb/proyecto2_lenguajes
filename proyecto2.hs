-- datos de la empresa
-- guardados como globales
nombre_empresa = "Los cleteros"
sitio_web = "www.cletaslocas.com"
contacto = 87456987
tarifa_pedal = 1000
tarifa_electronico = 2000

--Salidas:Ninguna
--Funcionalidad:Menú inicial introduciendo el usuario que permite acceder a las funcionalidades
inicio:: IO()
inicio= do
    putStr("Introduzca la ruta del archivo para cargar los parqueos:")
    ruta <- getLine
    parqueos <- leerParqueos ruta
    print("Datos de parqueos cargados")
    putStr("Introduzca la ruta del archivo para cargar los usuarios:")
    rutaUser <- getLine
    usuarios <- leerUsuarios rutaUser
    print("Datos de usuarios cargados")
    putStr("\nBienvenido\n")
    putStr("Introduzca el usuario:")
    usuario <- getLine
    case usuario of
        "admin" -> menu_principal(-1,parqueos,usuarios)
        
    return()
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las opciones operativos o generales
menu_principal(opcion, parqueos,usuarios)= do
    putStr("\nMenú Principal\n")

    putStr("1.Opciones Operativas\n")
    putStr("2.Opciones Generales \n")
    putStr("3.Salir \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1-> print("")
        1 -> menu_operativo(-1,parqueos,usuarios)
        2 -> menu_general(-1,parqueos,usuarios)
        3 -> return()
        
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú operativo para acceder a las funciones operativas
menu_operativo(opcion, parqueos,usuarios)= do
    putStr("\nMenú Operativo\n")
    putStr("1.Mostrar parqueos\n")
    putStr("2.Mostrar bicicletas \n")
    putStr("3.Mostrar usuarios \n")
    putStr("4.Estadísticas \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1 -> print("")
        1 -> showParqueos parqueos usuarios
        2 -> putStr("2")
        3 -> showUsuarios usuarios parqueos
        4 -> putStr("4")
        5 -> menu_principal(-1,parqueos,usuarios)
    return()

--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las funciones generales   
menu_general(opcion,parqueos,usuarios)= do
    putStr("\nMenú Generales\n")
    putStr("1.Consultar bicicletas\n")
    putStr("2.Alquiler\n")
    putStr("3.Facturar \n")
    putStr("4.Consulta de factura \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1 -> print("")
        1 -> putStr("1")
        2 -> putStr("2")
        3 -> putStr("3")
        4 -> putStr("4")
        5 -> menu_principal(-1,parqueos,usuarios)
    return()

--estructura para almacenar parqueos--------------------------------------------------
type Nombre_parqueo = String
type Direccion_parqueo = String
type Provincia = String
type X_parqueo = Integer
type Y_parqueo = Integer
data Parqueo = Parqueo Nombre_parqueo Direccion_parqueo Provincia X_parqueo Y_parqueo;
--------------------------------------------------------------------------------------

-- Constructor de parqueos---------------------------------------------------------------------------------------------------------------
creaParqueo(elemento) = Parqueo (elemento!!0) (elemento!!1) (elemento!!2) (read (elemento!!3) :: Integer) (read (elemento!!4) :: Integer)
getNombre_parqueo (Parqueo nombre_parqueo _ _ _ _) = nombre_parqueo;
getDireccion_parqueo (Parqueo _ direccion_parqueo _ _ _) = direccion_parqueo;
getProvincia (Parqueo _ _ provincia _ _) = provincia;
getX_parqueo (Parqueo _ _ _ x _) = x;
getY_parqueo (Parqueo _ _ _ _ y) = y;
-----------------------------------------------------------------------------------------------------------------------------------------

--Entradas:Una ruta de archivo
--Salidas:Una lista de parqueos
--Funcionalidad:Guarda el contenido del archivo
leerParqueos :: FilePath -> IO [Parqueo]
leerParqueos archivo = do
    contenido <- readFile archivo
    let parqueos = separaElementos (lines contenido)
    return parqueos

--Entradas:Una lista de listas de Char
--Salidas:Una lista de Parqueos
--Funcionalidad:se encarga de ir almacenando los parqueos ya bien estructurados
separaElementos :: [[Char]]-> [Parqueo]
separaElementos lista = 
    if null(lista) then []
    else
        [creaParqueo(separaPorComas((head lista),""))] ++ separaElementos (tail lista)

 --Entradas:Una tupla de listas de char
--Salidas:Una lista de listas de char
--Funcionalidad:se encarga de separar por comas el contenido enviado
separaPorComas :: ([Char],[Char])->[[Char]]
separaPorComas (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
            [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp++[(head cadena)])

--Entradas:Una lista de parqueos
--Salidas:No tiene
--Funcionalidad:se encarga pedir la provincia por buscar en los parqueos
showParqueos :: [Parqueo]->[Usuario] -> IO()
showParqueos lista usuarios = do
    putStr("Introduzca la provincia: ")
    temporal <- getLine
    showParqueosAux lista temporal
    menu_operativo(-1,lista,usuarios)

--Entradas:Una lista de parqueos y un string
--Salidas:No tiene
--Funcionalidad:se encarga de validar cuando se acaban los parqueos cargados
showParqueosAux :: [Parqueo] -> String -> IO ()
showParqueosAux [ ] prov = print("")
showParqueosAux lista prov=
    
    do  
            showParqueo (head lista) prov
            showParqueosAux (tail lista) prov

--Entradas:Un parqueo y un string
--Salidas:No tiene
--Funcionalidad:se encarga de imprimir la informacion del parqueo
showParqueo :: Parqueo -> String -> IO ()
showParqueo parqueo prov=
    let 
        nombre_parqueo = getNombre_parqueo(parqueo)
        direccion_parqueo = getDireccion_parqueo(parqueo)
        provincia = getProvincia(parqueo)
        x = getX_parqueo(parqueo)
        y = getY_parqueo(parqueo)
    in
        if provincia == prov then
            print("nombre: " ++ nombre_parqueo ++ ", direccion: " ++ direccion_parqueo ++ ", provincia: " ++ provincia ++ ", x: " ++ show x ++ ", y: " ++ show y)
        else
            return ()

--estructura para almacenar usuarios--------------------------------------------------
type Cedula = Integer
type Nombre_usuario = String
data Usuario = Usuario Cedula Nombre_usuario  ;
--------------------------------------------------------------------------------------

-- Constructor de usuario---------------------------------------------------------------------------------------------------------------
creaUsuario(elemento) = Usuario (read (elemento!!0) :: Integer) (elemento!!1) 
getCedula (Usuario cedula _ ) = cedula;
getNombre_usuario (Usuario _ nombre_usuario ) = nombre_usuario;
-----------------------------------------------------------------------------------------------------------------------------------------

--Entradas:Una ruta de archivo
--Salidas:Una lista de usuarios
--Funcionalidad:Guarda el contenido del usuario
leerUsuarios :: FilePath -> IO [Usuario]
leerUsuarios archivo = do
    contenido <- readFile archivo
    let usuarios = separaElementosUsuario (lines contenido)
    return usuarios

--Entradas:Una lista de listas de Char
--Salidas:Una lista de Parqueos
--Funcionalidad:se encarga de ir almacenando los parqueos ya bien estructurados
separaElementosUsuario :: [[Char]]-> [Usuario]
separaElementosUsuario lista = 
    if null(lista) then []
    else
        [creaUsuario(separaPorComas((head lista),""))] ++ separaElementosUsuario (tail lista)

showUsuarios :: [Usuario]->[Parqueo] -> IO()
showUsuarios lista parqueos = do
    putStr("Introduzca el número de cedula: ")
    temporal <- getLine
    showUsuariosAux lista temporal
    menu_operativo(-1,parqueos,lista)

--Entradas:Una lista de usuarios y un string
--Salidas:No tiene
--Funcionalidad:se encarga de validar cuando se acaban los usuarios cargados
showUsuariosAux :: [Usuario] -> String -> IO ()
showUsuariosAux [ ] cedul = print("")
showUsuariosAux lista cedul=
    
    do  
        let cedul_int=(read (cedul) :: Integer)
        
        if "#" == cedul then
            do
            showTodosUsuarios (head lista) 
            showUsuariosAux (tail lista) cedul
        else 
            do   
            showUsuario (head lista) cedul_int
            showUsuariosAux (tail lista) cedul

--Entradas:Un usuarios y un string
--Salidas:No tiene
--Funcionalidad:se encarga de imprimir la informacion de los usuarios
showUsuario :: Usuario -> Integer -> IO ()
showUsuario usuario cedul=
    let 
        cedula = getCedula(usuario)
        nombre_usuario = getNombre_usuario(usuario)
    in
        if cedula == cedul then
            print("Cedula: " ++ show cedula ++ ", Nombre del Usuario: " ++ nombre_usuario)
        else
            return ()
--Entradas:Un usuarios
--Salidas:No tiene
--Funcionalidad:se encarga de imprimir la informacion de todos los usuarios
showTodosUsuarios :: Usuario -> IO ()
showTodosUsuarios usuario =
    let 
        cedula = getCedula(usuario)
        nombre_usuario = getNombre_usuario(usuario)
    in

        print("Cedula: " ++ show cedula ++ ", Nombre del Usuario: " ++ nombre_usuario)
        
            