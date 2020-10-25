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
    putStr("Introduzca la ruta del archivo para cargar las bicicletas:")
    ruta <- getLine
    bicicletas <- leerBicicletas ruta
    print("Datos de bicicletas cargados")
    putStr("Introduzca la ruta del archivo para cargar los usuarios:")
    rutaUser <- getLine
    usuarios <- leerUsuarios rutaUser
    print("Datos de usuarios cargados")
    putStr("\nBienvenido\n")
    putStr("Introduzca el usuario:")
    usuario <- getLine
    case usuario of
        "admin" -> menu_principal(-1,parqueos,bicicletas,usuarios)
        
    return()
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las opciones operativos o generales
menu_principal(opcion, parqueos,bicicletas,usuarios)= do
    putStr("\nMenú Principal\n")

    putStr("1.Opciones Operativas\n")
    putStr("2.Opciones Generales \n")
    putStr("3.Salir \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1-> print("")
        1 -> menu_operativo(-1,parqueos,bicicletas,usuarios)
        2 -> menu_general(-1,parqueos,bicicletas,usuarios)
        3 -> return()
        
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú operativo para acceder a las funciones operativas
menu_operativo(opcion, parqueos,bicicletas,usuarios)= do
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
        1 -> showParqueos parqueos bicicletas usuarios
        2 -> putStr("2")
        3 -> showUsuarios usuarios parqueos bicicletas
        4 -> putStr("4")
        5 -> menu_principal(-1,parqueos,bicicletas,usuarios)
    return()

--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las funciones generales   
menu_general(opcion,parqueos,bicicletas,usuarios)= do
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
        1 -> consultaBicicletas parqueos bicicletas usuarios
        2 -> putStr("2")
        3 -> putStr("3")
        4 -> putStr("4")
        5 -> menu_principal(-1,parqueos,bicicletas,usuarios)
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
showParqueos :: [Parqueo]->[Bicicleta]->[Usuario] -> IO()
showParqueos listaparqueos listabicicletas listausuarios = do
    putStr("Introduzca la provincia: ")
    temporal <- getLine
    showParqueosAux listaparqueos listabicicletas temporal
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios)

--Entradas:Una lista de parqueos y un string
--Salidas:No tiene
--Funcionalidad:se encarga de validar cuando se acaban los parqueos cargados
showParqueosAux :: [Parqueo] ->[Bicicleta]-> String -> IO ()
showParqueosAux [ ] listabicicletas prov = print("")
showParqueosAux listaparqueos listabicicletas prov=
    
    do  
            showParqueo (head listaparqueos) listabicicletas prov
            showParqueosAux (tail listaparqueos) listabicicletas prov

--Entradas:Un parqueo y un string
--Salidas:No tiene
--Funcionalidad:se encarga de imprimir la informacion del parqueo
showParqueo :: Parqueo ->[Bicicleta]-> String -> IO ()
showParqueo parqueo listabicicletas prov=
    let 
        nombre_parqueo = getNombre_parqueo(parqueo)
        direccion_parqueo = getDireccion_parqueo(parqueo)
        provincia = getProvincia(parqueo)
        x = getX_parqueo(parqueo)
        y = getY_parqueo(parqueo)
    in
        if provincia == prov then
            do
            print("nombre: " ++ nombre_parqueo ++ ", direccion: " ++ direccion_parqueo ++ ", provincia: " ++ provincia ++ ", x: " ++ show x ++ ", y: " ++ show y)
            showBicicletasAux listabicicletas nombre_parqueo
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

showUsuarios :: [Usuario]->[Parqueo] ->[Bicicleta]-> IO()
showUsuarios listausuarios listaparqueos listabicicletas= do
    putStr("Introduzca el número de cedula: ")
    temporal <- getLine
    showUsuariosAux listausuarios temporal
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios)

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

--estructura para almacenar bicicletas------------------------------------------------
type Codigo_bicicleta = String
type Tipo_bicicleta  = String
type Ubicacion_bicicleta = String
type Transito_bicicleta = String
data Bicicleta = Bicicleta Codigo_bicicleta Tipo_bicicleta Ubicacion_bicicleta Transito_bicicleta;
--------------------------------------------------------------------------------------

-- Constructor de bicicletas---------------------------------------------------------------------------------------------------------------
creaBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1) (elemento!!2) (elemento!!3)
getCodigo_bicicleta (Bicicleta codigo_bicicleta _ _ _) = codigo_bicicleta;
getTipo_bicicleta (Bicicleta _ tipo_bicicleta _ _) = tipo_bicicleta;
getUbicacion_bicicleta (Bicicleta _ _ ubicacion_bicicleta _) = ubicacion_bicicleta;
getTransito_bicicleta (Bicicleta _ _ _ transito_bicicleta) = transito_bicicleta;
-----------------------------------------------------------------------------------------------------------------------------------------

--Entradas:Una ruta de archivo
--Salidas:Una lista de parqueos
--Funcionalidad:Guarda el contenido del archivo
leerBicicletas :: FilePath -> IO [Bicicleta]
leerBicicletas archivo = do
    contenido <- readFile archivo
    let bicicletas = separaElementosBicicleta (lines contenido)
    return bicicletas

separaElementosBicicleta :: [[Char]]-> [Bicicleta]
separaElementosBicicleta lista = 
    if null(lista) then []
    else
        [creaBicicleta(separaPorComas((head lista),"") ++ ["inactivo"])] ++ separaElementosBicicleta (tail lista)

showBicicletas :: [Bicicleta] ->[Parqueo]->[Usuario]-> IO()
showBicicletas listabicicletas listaparqueos listausuarios = do
    putStr("Introduzca el nombre del parqueo: ")
    temporal <- getLine
    showBicicletasAux listabicicletas temporal
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios)

showBicicletasAux :: [Bicicleta] -> String -> IO ()
showBicicletasAux [ ] nombre = print("")
showBicicletasAux lista nombre=
    
    do  
            showBicicleta (head lista) nombre
            showBicicletasAux (tail lista) nombre

showBicicleta :: Bicicleta -> String -> IO ()
showBicicleta bicicleta nombre=
    let 
        codigo_bicicleta = getCodigo_bicicleta(bicicleta)
        tipo_bicicleta = getTipo_bicicleta(bicicleta)
        ubicacion_bicicleta = getUbicacion_bicicleta(bicicleta)
        transito_bicicleta = getTransito_bicicleta(bicicleta)
    in
        if transito_bicicleta == "activo" then
            if nombre == "transito" then
                print("codigo de bicicleta: " ++ codigo_bicicleta ++ ", tipo de bicicleta: " ++ tipo_bicicleta ++ ", ubicacion: " ++ ubicacion_bicicleta)
            else 
                return ()
        else
            if nombre == "#" then
                print("codigo de bicicleta: " ++ codigo_bicicleta ++ ", tipo de bicicleta: " ++ tipo_bicicleta ++ ", ubicacion: " ++ ubicacion_bicicleta)
            else
                if nombre == ubicacion_bicicleta then
                    print("codigo de bicicleta: " ++ codigo_bicicleta ++ ", tipo de bicicleta: " ++ tipo_bicicleta ++ ", ubicacion: " ++ ubicacion_bicicleta)
                else
                    return ()

consultaBicicletas :: [Parqueo] -> [Bicicleta] -> [Usuario] -> IO()
consultaBicicletas listaparqueos listabicicletas listausuarios = do
    putStr("Introduzca su coordenada x: ")
    x_str <- getLine
    let x = (read x_str :: Integer)
    putStr("Introduzca su coordenada y: ")
    y_str <- getLine
    let y = (read y_str :: Integer)
    let nombre_parqueo = sacaBajo (consultaBicicletasAux listaparqueos x y)
    imprimeParqueo listaparqueos listabicicletas nombre_parqueo
    menu_general(-1,listaparqueos,listabicicletas,listausuarios)

consultaBicicletasAux :: [Parqueo] -> Integer-> Integer->[(String,Integer)]
consultaBicicletasAux [ ] x y = []
consultaBicicletasAux listaparqueos x y =
    let
        elemento = calculaPitagoras (head listaparqueos) x y
    in
        [elemento] ++ consultaBicicletasAux (tail listaparqueos) x y

calculaPitagoras :: Parqueo -> Integer -> Integer->(String,Integer)
calculaPitagoras parqueo x_us y_us =
    let 
        nombre_parqueo = getNombre_parqueo(parqueo)
        x = getX_parqueo(parqueo)
        y = getY_parqueo(parqueo)
        res = (x_us - x)^2 + (y_us - y)^2
    in
        (nombre_parqueo,res)

sacaBajo :: [(String,Integer)]->String
sacaBajo lista =
    do  
        sacaBajoAux (head lista) (tail lista)

sacaBajoAux :: (String,Integer)->[(String,Integer)]->String
sacaBajoAux (a,b) lista =
    let 
        (c,d) = (head lista)
    in
        if null(lista) then a
        else
            if b < d then
                sacaBajoAux (a,b) (tail lista)
            else
                sacaBajoAux (c,d) (tail lista)

imprimeParqueo :: [Parqueo]->[Bicicleta]->String->IO()
imprimeParqueo [ ] listabicicletas nombre = print("")
imprimeParqueo listaparqueos listabicicletas nombre=
    
    do  
            imprimeParqueoAux (head listaparqueos) listabicicletas nombre
            imprimeParqueo (tail listaparqueos) listabicicletas nombre

imprimeParqueoAux :: Parqueo->[Bicicleta]->String->IO()
imprimeParqueoAux parqueo listabicicletas nombre =
    let 
        nombre_parqueo = getNombre_parqueo(parqueo)
        direccion_parqueo = getDireccion_parqueo(parqueo)
        provincia = getProvincia(parqueo)
        x = getX_parqueo(parqueo)
        y = getY_parqueo(parqueo)
    in
        if nombre == nombre_parqueo then
            do
            print("nombre: " ++ nombre_parqueo ++ ", direccion: " ++ direccion_parqueo ++ ", provincia: " ++ provincia ++ ", x: " ++ show x ++ ", y: " ++ show y)
            showBicicletasAux listabicicletas nombre_parqueo
        else
            return ()