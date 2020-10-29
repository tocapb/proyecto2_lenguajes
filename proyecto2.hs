import Data.List
import System.Exit (exitSuccess)
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
    bicicleticas <- leerBicicletas ruta
    let bicicletas = verificaParqueos bicicleticas parqueos
    print("Datos de bicicletas cargados")
    putStr("Introduzca la ruta del archivo para cargar los usuarios:")
    rutaUser <- getLine
    usuarios <- leerUsuarios rutaUser
    print("Datos de usuarios cargados")
    putStr("\nBienvenido\n")
    putStr("Introduzca el usuario:")
    usuario <- getLine
    let alquileres=[]
    let facturas=[]
    case usuario of
        "admin" -> menu_principal(-1,parqueos,bicicletas,usuarios,alquileres, facturas)
        
    return()
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las opciones operativos o generales
menu_principal(opcion, parqueos,bicicletas,usuarios, alquileres, facturas)= do
    putStr("\nMenú Principal\n")

    putStr("1.Opciones Operativas\n")
    putStr("2.Opciones Generales \n")
    putStr("3.Salir \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1-> print("")
        1 -> menu_operativo(-1,parqueos,bicicletas,usuarios,alquileres,facturas)
        2 -> menu_general(-1,parqueos,bicicletas,usuarios, alquileres, facturas)
        3 -> exitSuccess
        
--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú operativo para acceder a las funciones operativas
menu_operativo(opcion, parqueos,bicicletas,usuarios,alquileres, facturas)= do
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
        1 -> showParqueos parqueos bicicletas usuarios alquileres facturas
        2 -> showBicicletas bicicletas parqueos usuarios alquileres facturas
        3 -> showUsuarios usuarios parqueos bicicletas alquileres facturas
        4 -> menu_estadisticas (-1,parqueos,bicicletas,usuarios,alquileres,facturas)
        5 -> menu_principal(-1,parqueos,bicicletas,usuarios, alquileres, facturas)
    return()

--Entradas:Una opcion de tipo entero y una lista de tipo Parqueo
--Salidas:Ninguna
--Funcionalidad:Menú principal para acceder a las funciones generales   
menu_general(opcion,parqueos,bicicletas,usuarios,alquileres, facturas)= do
    putStr("\nMenú Generales\n")
    putStr("1.Consultar bicicletas\n")
    putStr("2.Alquilar\n")
    putStr("3.Facturar \n")
    putStr("4.Consulta de factura \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1 -> print("")
        1 -> consultaBicicletas parqueos bicicletas usuarios alquileres facturas
        2 -> alquilarBicicletas parqueos bicicletas usuarios alquileres facturas
        3 -> imprimeFacturas bicicletas parqueos  usuarios alquileres facturas
        4 -> pideIdfactura bicicletas parqueos  usuarios alquileres facturas
        5 -> menu_principal(-1,parqueos,bicicletas,usuarios, alquileres, facturas)
    return()

menu_estadisticas(opcion,parqueos,bicicletas,usuarios,alquileres,facturas)= do
    putStr("\nEstadisticas\n")
    putStr("1.Top 5 usuarios con más viajes\n")
    putStr("2.Top 5 parqueos con más viajes \n")
    putStr("3.Top 3 de bicicletas con más kilometros recorridos \n")
    putStr("4.Resumen \n")
    putStr("5.Volver \n")
    putStr("Introduzca su opción:")
    temporal <- getLine
    let opcion = (read temporal :: Integer)
    case opcion of
        -1 -> print("")
        1 -> imprimeEstadistica (estadisticaInicioAux usuarios alquileres) 1 parqueos bicicletas usuarios alquileres facturas
        2 -> imprimeEstadistica2(estadisticaInicio2Aux parqueos alquileres) 1 parqueos bicicletas usuarios alquileres facturas
        3 -> imprimeEstadistica3(estadisticaInicio3Aux bicicletas facturas) 1 parqueos bicicletas usuarios alquileres facturas
        4 -> imprimeEstadistica4(estadisticaInicio4Aux facturas 0) parqueos bicicletas usuarios alquileres facturas
        5 -> menu_operativo(-1,parqueos,bicicletas,usuarios, alquileres,facturas)
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
showParqueos :: [Parqueo]->[Bicicleta]->[Usuario]->[Alquiler]->[Factura]-> IO()
showParqueos listaparqueos listabicicletas listausuarios listaalquileres listafacturas= do
    putStr("Introduzca la provincia: ")
    temporal <- getLine
    showParqueosAux listaparqueos listabicicletas temporal
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres,listafacturas)

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
        [creaUsuario(separaPorComasUsuarios((head lista),""))] ++ separaElementosUsuario (tail lista)

showUsuarios :: [Usuario]->[Parqueo] ->[Bicicleta]-> [Alquiler]->[Factura]-> IO()
showUsuarios listausuarios listaparqueos listabicicletas listaalquileres listafacturas= do
    putStr("Introduzca el número de cedula: ")
    temporal <- getLine
    showUsuariosAux listausuarios temporal listaalquileres
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres, listafacturas)

--Entradas:Una lista de usuarios y un string
--Salidas:No tiene
--Funcionalidad:se encarga de validar cuando se acaban los usuarios cargados
showUsuariosAux :: [Usuario] -> String-> [Alquiler] -> IO ()
showUsuariosAux [ ] cedul listaalquileres = print("")
showUsuariosAux lista cedul listaalquileres=
    
    do  
        let cedul_int=(read (cedul) :: Integer)
        
        if "#" == cedul then
            do
            showTodosUsuarios (head lista) 
            showUsuariosAux (tail lista) cedul listaalquileres
        else 
            do   
            showUsuario (head lista) cedul_int listaalquileres
            showUsuariosAux (tail lista) cedul listaalquileres

--Entradas:Un usuarios y un string
--Salidas:No tiene
--Funcionalidad:se encarga de imprimir la informacion de los usuarios
showUsuario :: Usuario -> Integer->[Alquiler] -> IO ()
showUsuario usuario cedul listaalquileres=
    let 
        cedula = getCedula(usuario)
        nombre_usuario = getNombre_usuario(usuario)
    in
        if cedula == cedul then 
            do
                print("Cedula: " ++ show cedula ++ ", Nombre del Usuario: " ++ nombre_usuario)
                showAlquileresUsuarioAux listaalquileres cedul
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


showAlquileresUsuarioAux :: [Alquiler]->Integer->IO ()
showAlquileresUsuarioAux [ ] cedul = print("")
showAlquileresUsuarioAux lista cedul =
    do
        showAlquileresUsuario (head lista) cedul
        showAlquileresUsuarioAux (tail lista) cedul

showAlquileresUsuario :: Alquiler -> Integer -> IO ()
showAlquileresUsuario alquiler cedul=
    let 
        cedula= getUsuario_cedula(alquiler)
        id_cleta = getId_bicicleta(alquiler)
        id_alqui = getId_alquiler(alquiler)
        estado = getEstado_alquiler(alquiler)
        salida= getSalida_alquiler(alquiler)
        llegada= getDestino_alquiler(alquiler)
        tipo = getTipo_bici(alquiler)
    in
        if cedula == cedul then 
            do
                print("Alquiler= " ++ " Cleta: " ++ id_cleta ++", Alqui: " ++ show id_alqui++ ", Estado: "++ estado ++", Salida: "++ salida++", Destino: "++ llegada)

        else
            return ()        

separaPorComasUsuarios :: ([Char],[Char])->[[Char]]
separaPorComasUsuarios (cadena, temp) =
    if cadena == "" then [temp]
    else
        if (head cadena) == (head ",") then
                [temp] ++ separaPorComas ((tail cadena), "")
        else
            separaPorComas ((tail cadena), temp++[(head cadena)])
--estructura para almacenar bicicletas------------------------------------------------
type Codigo_bicicleta = String
type Tipo_bicicleta  = String
type Ubicacion_bicicleta = String
data Bicicleta = Bicicleta Codigo_bicicleta Tipo_bicicleta Ubicacion_bicicleta;
--------------------------------------------------------------------------------------

-- Constructor de bicicletas---------------------------------------------------------------------------------------------------------------
creaBicicleta(elemento) = Bicicleta (elemento!!0) (elemento!!1) (elemento!!2)
getCodigo_bicicleta (Bicicleta codigo_bicicleta _ _ ) = codigo_bicicleta;
getTipo_bicicleta (Bicicleta _ tipo_bicicleta _ ) = tipo_bicicleta;
getUbicacion_bicicleta (Bicicleta _ _ ubicacion_bicicleta ) = ubicacion_bicicleta;

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
        [creaBicicleta(separaPorComas((head lista),""))] ++ separaElementosBicicleta (tail lista)

verificaParqueos :: [Bicicleta]->[Parqueo]->[Bicicleta]
verificaParqueos listabicicletas listaparqueos =
    let
        ubicacion_bicicleta = getUbicacion_bicicleta(head listabicicletas)
    in
        if null(listabicicletas) then []
        else
            if verificaParqueosAux listaparqueos ubicacion_bicicleta == 1 then 
                [head listabicicletas] ++ verificaParqueos (tail listabicicletas) listaparqueos
            else
                verificaParqueos (tail listabicicletas) listaparqueos

verificaParqueosAux :: [Parqueo]->String->Integer
verificaParqueosAux [ ] ubicacion = 2
verificaParqueosAux listaparqueos ubicacion = 
    let
        nombre_parqueo = getNombre_parqueo(head listaparqueos)
    in
        if nombre_parqueo == ubicacion then
            1
        else
            verificaParqueosAux (tail listaparqueos) ubicacion

showBicicletas :: [Bicicleta] ->[Parqueo]->[Usuario]->[Alquiler]->[Factura]-> IO()
showBicicletas listabicicletas listaparqueos listausuarios listaalquileres listafacturas= do
    putStr("Introduzca el nombre del parqueo: ")
    temporal <- getLine
    showBicicletasAux listabicicletas temporal
    menu_operativo(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres, listafacturas)

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
  
    in
        if ubicacion_bicicleta == "en transito" then
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

consultaBicicletas :: [Parqueo] -> [Bicicleta] -> [Usuario]->[Alquiler]->[Factura]-> IO()
consultaBicicletas listaparqueos listabicicletas listausuarios listaalquileres listafacturas= do
    putStr("Introduzca su coordenada x: ")
    x_str <- getLine
    let x = (read x_str :: Integer)
    putStr("Introduzca su coordenada y: ")
    y_str <- getLine
    let y = (read y_str :: Integer)
    let nombre_parqueo = sacaBajo (consultaBicicletasAux listaparqueos x y)
    imprimeParqueo listaparqueos listabicicletas nombre_parqueo
    menu_general(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres, listafacturas)

consultaBicicletasAlquiler :: [Parqueo] -> [Bicicleta] -> [Usuario] -> IO()
consultaBicicletasAlquiler listaparqueos listabicicletas listausuarios = do
    putStr("Introduzca su coordenada x: ")
    x_str <- getLine
    let x = (read x_str :: Integer)
    putStr("Introduzca su coordenada y: ")
    y_str <- getLine
    let y = (read y_str :: Integer)
    let nombre_parqueo = sacaBajo (consultaBicicletasAux listaparqueos x y)
    imprimeParqueo listaparqueos listabicicletas nombre_parqueo
    

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


---------------------------------------------Alquiler--------------------------------------------------------------
--estructura para almacenar alquileres------------------------------------------------
type Usuario_cedula = Integer
type Id_bicicleta  = String
type Id_alquiler= Integer
type Estado_alquiler= String
type Destino_alquiler= String
type Salida_Alquiler = String
type Tipo_bici = String
data Alquiler = Alquiler Usuario_cedula Id_bicicleta Id_alquiler Estado_alquiler Destino_alquiler Salida_Alquiler Tipo_bici;
--------------------------------------------------------------------------------------

-- Constructor de Alquiler--------------------------------------------------------------------------------------------------------------
creaAlquiler(elemento) = Alquiler (read (elemento!!0) :: Integer) (elemento!!1) (read (elemento!!2) :: Integer) (elemento!!3) (elemento!!4) (elemento!!5) (elemento!!6)
getUsuario_cedula (Alquiler usuario_cedula _ _ _ _ _ _ ) = usuario_cedula;
getId_bicicleta (Alquiler _ id_bicicleta _ _ _ _ _) = id_bicicleta;
getId_alquiler (Alquiler _ _ id_alquiler _ _ _ _) = id_alquiler;
getEstado_alquiler (Alquiler _ _ _ estado_alquiler _ _ _ ) = estado_alquiler;
getDestino_alquiler (Alquiler _ _ _ _ destino_alquiler _ _) = destino_alquiler;
getSalida_alquiler (Alquiler _ _ _ _ _ salida_alquiler _) = salida_alquiler;
getTipo_bici (Alquiler _ _ _ _ _ _ tipo_bici) = tipo_bici;
-------------------------------------------------------------------------------------------------------------------------------------------

alquilarBicicletas :: [Parqueo] -> [Bicicleta] -> [Usuario]-> [Alquiler]->[Factura]-> IO()
alquilarBicicletas listaparqueos listabicicletas listausuarios listaalquileres listafacturas= do
    putStr("Introduzca su Cédula: ")
    cedula_str <- getLine
    let validando = validarCedulaAux listausuarios cedula_str
    
    if validando == Nothing then
        do
            putStr("Usuario Inválido")
            menu_general(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres, listafacturas)    
    else 
        do
            putStr("Introduzca su punto de salida\n")
            consultaBicicletasAlquiler listaparqueos listabicicletas listausuarios
            putStr("Introduzca el parqueo de llegada\n")
            parqueo <- getLine
            let validando = validarParqueoAux listaparqueos parqueo
            if validando == Nothing then 
                do
                    putStr("Parqueo Inválido\n")
            else
                do 
                    putStr("Introduzca código de la bicicleta\n")
                    codigo_bici <- getLine
                    let validando = validarCodigoAux listabicicletas codigo_bici
                    if (head validando)== " " || (head validando) == parqueo then
                        do 
                            putStr("Código Inválido o Parqueo de salida igual al se llegada\n")
                            
                    else
                        do 
                            
                            let listaalquileresA= crearAlquiler listaalquileres cedula_str codigo_bici parqueo (head validando) (last(validando))
                            let listabicicletasA=modificarCodigoAux listabicicletas codigo_bici 

                            putStr("Procesando Alquiler...\n")
                            menu_general(-1,listaparqueos,listabicicletasA,listausuarios, listaalquileresA, listafacturas)
                            
                    
                    


    menu_general(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres, listafacturas)

validarCedulaAux :: [Usuario] -> String -> Maybe Int
validarCedulaAux lista cedul=
    
    do  
        let cedul_int=(read (cedul) :: Integer)
            bandera=1
               
        
        if null(lista) then 
            Nothing
        else
            if validarCedula (head lista) cedul_int == Nothing then
                validarCedulaAux (tail lista) cedul
            else
                Just bandera

validarCedula :: Usuario -> Integer -> Maybe Int
validarCedula usuario cedul=
    let 
        cedula = getCedula(usuario)
        bandera=1
    in
        if cedula == cedul then
            Just bandera
        else
            Nothing


validarParqueoAux :: [Parqueo] -> String -> Maybe Int
validarParqueoAux lista parqueito=
    
    do  
        let bandera=1
               
        
        if null(lista) then 
            Nothing
        else
            if validarParqueo (head lista) parqueito == Nothing then
                validarParqueoAux (tail lista) parqueito
            else
                Just bandera

validarParqueo :: Parqueo -> String -> Maybe Int
validarParqueo parqueo parqueito=
    let 
        nombre_parqueito = getNombre_parqueo(parqueo)
        bandera=1
    in
        if nombre_parqueito == parqueito then
            Just bandera
        else
            Nothing

validarCodigoAux :: [Bicicleta] -> String -> [String]
validarCodigoAux lista bici=
    
    do  
        let bandera= validarCodigo (head lista) bici
               
        
        if null(lista) then 
            [" "]
        else
            if validarCodigo (head lista) bici == [" "] then
                validarCodigoAux (tail lista) bici
            else
                bandera

validarCodigo :: Bicicleta -> String -> [String]
validarCodigo bicicleta bici=
    let 
        cod_bici = getCodigo_bicicleta(bicicleta)
        ubi_bici = getUbicacion_bicicleta(bicicleta)
        tipo_bici = getTipo_bicicleta(bicicleta)
        
    in
        if cod_bici == bici && ubi_bici/= "en transito" then 
            [ubi_bici,tipo_bici]
        else
            [" "]

modificarCodigoAux :: [Bicicleta] -> String -> [Bicicleta]
modificarCodigoAux lista bici=
    
    do  
        if null(lista) then 
            []
        else
            let 
                cod_bici = getCodigo_bicicleta(head lista)
                tip_bici = getTipo_bicicleta(head lista)
                ubi_bici= "en transito"
            in
                if cod_bici == bici then 
                    [creaBicicleta([cod_bici,tip_bici,ubi_bici])] ++ (tail lista)
                else
                    [head lista] ++ modificarCodigoAux (tail lista) bici


crearAlquiler ::[Alquiler] -> String -> String -> String ->String -> String->[Alquiler]
crearAlquiler lista usuario id_bici desti_alqui salid_alqui tipo_bici= 
    if null(lista) then 
        [creaAlquiler([usuario,id_bici,"1","activo",desti_alqui, salid_alqui, tipo_bici])]
    else
        do
            let id_alquiler=getId_alquiler(last(lista))+1
            init(lista)++[last(lista)] ++  [creaAlquiler([usuario,id_bici,show id_alquiler,"activo",desti_alqui,salid_alqui,tipo_bici])]

---------------------------------------------Factura----------------------------------
--estructura para almacenar facturas------------------------------------------------
type Id_alquiler_factura = Integer
type Id_bicicleta_factura  = String
type Tipo_bici_factura = String
type Estado_factura = String
type Destino_factura = String
type Salida_factura = String
type Distancia_recorrido = Integer
type Id_factura = Integer
data Factura = Factura Id_alquiler_factura Id_bicicleta_factura Tipo_bici_factura Estado_factura Destino_factura Salida_factura Distancia_recorrido Id_factura;
--------------------------------------------------------------------------------------

-- Constructor de Factura--------------------------------------------------------------------------------------------------------------
creaFactura(elemento) = Factura (read (elemento!!0) :: Integer) (elemento!!1) (elemento!!2) (elemento!!3) (elemento!!4) (elemento!!5) (read (elemento!!6) :: Integer) (read (elemento!!7) :: Integer) 
getId_alquiler_factura (Factura id_alquiler_factura _ _ _ _ _ _ _ ) = id_alquiler_factura;
getId_bicicleta_factura (Factura _ id_bicicleta_factura _ _ _ _ _ _) = id_bicicleta_factura;
getTipo_bici_factura (Factura _ _ tipo_bici_factura _ _ _ _ _ ) = tipo_bici_factura;
getEstado_factura (Factura _ _ _ estado_factura _ _ _ _ ) = estado_factura;
getDestino_factura (Factura _ _ _ _ destino_factura _ _ _ ) = destino_factura;
getSalida_factura (Factura _ _ _ _ _ salida_factura _ _ ) = salida_factura;
getDistancia_recorrido (Factura _ _ _ _ _ _ distancia_recorrido _ ) = distancia_recorrido;
getId_factura (Factura _ _ _ _ _ _ _ id_factura ) = id_factura;
-------------------------------------------------------------------------------------------------------------------------------------------

imprimeFacturas :: [Bicicleta]->[Parqueo]->[Usuario]->[Alquiler]-> [Factura]->IO()
imprimeFacturas listabicicletas listaparqueos listausuarios listaalquileres listafacturas= do
    putStr("Introduzca su identificador de alquiler: ")
    temporal <- getLine
    let tmp = (read temporal :: Integer)

    let 
        listafacturastmp = listafacturas ++ imprimeFacturasAux listaalquileres listaparqueos listafacturas tmp
        id_alquiler = getId_alquiler_factura(last listafacturastmp)
        id_factura = getId_factura(last listafacturastmp)
        id_bicicleta = getId_bicicleta_factura(last listafacturastmp)
        destino = getDestino_factura(last listafacturastmp)
        listaalquilerestmp = modificarAlquiler listaalquileres id_alquiler
        listabicicletastmp = modificarCleta listabicicletas id_bicicleta destino

    verFacturas listafacturastmp id_factura

    menu_general(-1,listaparqueos,listabicicletastmp,listausuarios, listaalquilerestmp,listafacturastmp)

imprimeFacturasAux :: [Alquiler] ->[Parqueo]->[Factura]->Integer -> [Factura]
imprimeFacturasAux [ ] listaparqueos listafacturas identificador = []
imprimeFacturasAux listaalquileres listaparqueos listafacturas identificador=
    let 
        id_alquiler = getId_alquiler(head listaalquileres)
        estado_alquiler = getEstado_alquiler(head listaalquileres)
    in
        if estado_alquiler == "activo" then
            if id_alquiler == identificador then
                [showFactura (head listaalquileres) listaparqueos listafacturas] ++ imprimeFacturasAux (tail listaalquileres) listaparqueos listafacturas identificador
            else
                imprimeFacturasAux (tail listaalquileres) listaparqueos listafacturas identificador
        else
            imprimeFacturasAux (tail listaalquileres) listaparqueos listafacturas identificador

showFactura :: Alquiler -> [Parqueo]->[Factura]-> Factura
showFactura alquiler listaparqueos listafacturas=
    let 
        id_alquiler = getId_alquiler(alquiler)
        codigo_bicicleta = getId_bicicleta(alquiler)
        tipo_bicicleta = getTipo_bici(alquiler)
        estado_alquiler = getEstado_alquiler(alquiler)
        salida_alquiler = getSalida_alquiler(alquiler)
        destino_alquiler = getDestino_alquiler(alquiler)
        distancia_recorrido = calculaKm listaparqueos destino_alquiler salida_alquiler
        factura = creaFactura ([show id_alquiler,codigo_bicicleta,tipo_bicicleta,estado_alquiler,destino_alquiler,salida_alquiler,show distancia_recorrido,"1"])
        id_factura=getId_factura(last(listafacturas))+1
        factura2 = creaFactura ([show id_alquiler,codigo_bicicleta,tipo_bicicleta,estado_alquiler,destino_alquiler,salida_alquiler,show distancia_recorrido,show id_factura])
    in
        if null(listafacturas) then
            factura
        else
            factura2

calculaKm :: [Parqueo] -> String -> String->Integer
calculaKm listaparqueos des sal = 
    let
        parqueos = creaParParqueos listaparqueos des sal
        x_1 = getX_parqueo(head parqueos)
        y_1 = getY_parqueo(head parqueos)
        x_2 = getX_parqueo(last parqueos)
        y_2 = getY_parqueo(last parqueos)
        res = (x_1 - x_2)^2 + (y_1 - y_2)^2
    in
        res
    
creaParParqueos :: [Parqueo]->String->String->[Parqueo]
creaParParqueos [] des sal = []
creaParParqueos listaparqueos des sal=
    let 
        nombre_parqueo = getNombre_parqueo(head listaparqueos)
    in
        if nombre_parqueo == des ||  nombre_parqueo == sal then
            [head listaparqueos] ++ creaParParqueos (tail listaparqueos) des sal
        else
            creaParParqueos (tail listaparqueos) des sal

pideIdfactura :: [Bicicleta]->[Parqueo]->[Usuario]->[Alquiler]-> [Factura]->IO()
pideIdfactura listabicicletas listaparqueos listausuarios listaalquileres listafacturas= do
    putStr("Introduzca su identificador de factura: ")
    temporal <- getLine
    let tmp = (read temporal :: Integer)
    verFacturas listafacturas tmp
    menu_general(-1,listaparqueos,listabicicletas,listausuarios, listaalquileres,listafacturas)

verFacturas :: [Factura] ->Integer -> IO()
verFacturas []  identificador = print("")
verFacturas listafacturas identificador =
    do  
            verFacturasAux (head listafacturas) identificador
            verFacturas (tail listafacturas) identificador

verFacturasAux :: Factura ->  Integer -> IO()
verFacturasAux factura indicador =
    let 
        id_alquiler_factura = getId_alquiler_factura(factura)
        id_bicicleta_factura = getId_bicicleta_factura(factura)
        tipo_bici_factura = getTipo_bici_factura(factura)
        destino_factura = getDestino_factura(factura)
        salida_factura = getSalida_factura(factura)
        distancia_recorrido = getDistancia_recorrido(factura)
        id_factura = getId_factura(factura)
    in
        if id_factura == indicador then
            if tipo_bici_factura == "TR" then
                do
                    putStr("\n-------------------\n")
                    putStr("|Factura numero: " ++ show id_factura ++"|\n")
                    putStr("-------------------\n")
                    putStr("\nDetalles de bicicleta utilizada\n")
                    putStr("Identificador de su bicicleta: " ++ id_bicicleta_factura ++ "\n")
                    putStr("Tipo de bicicleta: tradicional\n")
                    putStr("Tarifa de dicho tipo es: " ++ show tarifa_pedal ++ " colones\n")
                    putStr("\nDetalles de viaje\n")
                    putStr("Estacionamiento de salida: " ++ salida_factura)
                    putStr("\nEstacionamiento de destino: " ++ destino_factura)
                    putStr("\nEl recorrido corresponde a: " ++ show distancia_recorrido ++ " kilometros\n")
                    putStr("\n----------------------------\n")
                    putStr("Monto total: " ++ show ( distancia_recorrido * tarifa_pedal ) ++ " colones"  )
                    putStr("\n----------------------------\n")
                    putStr("\nGracias por viajar con " ++ nombre_empresa)
                    putStr("\n-----------------------------------\n")
                    putStr("|Para consultas o informacion" ++ "     |\n")
                    putStr("|visitenos en " ++ sitio_web ++ " |\n")
                    putStr("|o llamenos al " ++ show contacto ++ "           |\n")
                    putStr("-----------------------------------\n")
            else
                do
                    putStr("\n-------------------\n")
                    putStr("|Factura numero: " ++ show id_factura ++"|\n")
                    putStr("-------------------\n")
                    putStr("\nDetalles de bicicleta utilizada\n")
                    putStr("Identificador de su bicicleta: " ++ id_bicicleta_factura ++ "\n")
                    putStr("Tipo de bicicleta: con asistencia electrica\n")
                    putStr("Tarifa de dicho tipo es: " ++ show tarifa_electronico ++ " colones\n")
                    putStr("\nDetalles de viaje\n")
                    putStr("Estacionamiento de salida: " ++ salida_factura)
                    putStr("\nEstacionamiento de destino: " ++ destino_factura)
                    putStr("\nEl recorrido corresponde a: " ++ show distancia_recorrido ++ " kilometros\n")
                    putStr("\n----------------------------\n")
                    putStr("Monto total: " ++ show ( distancia_recorrido * tarifa_electronico ) ++ " colones"  )
                    putStr("\n----------------------------\n")
                    putStr("\nGracias por viajar con " ++ nombre_empresa)
                    putStr("\n-----------------------------------\n")
                    putStr("|Para consultas o informacion" ++ "     |\n")
                    putStr("|visitenos en " ++ sitio_web ++ " |\n")
                    putStr("|o llamenos al " ++ show contacto ++ "           |\n")
                    putStr("-----------------------------------\n")
        else
            return()

modificarAlquiler :: [Alquiler]->Integer->[Alquiler]
modificarAlquiler listaalquileres id_alquiler=
    do  
        if null(listaalquileres) then 
            []
        else
            let 
                usuario = getUsuario_cedula(head listaalquileres)
                id_cleta = getId_bicicleta(head listaalquileres)
                id_alqui = getId_alquiler(head listaalquileres)
                salida= getSalida_alquiler(head listaalquileres)
                llegada= getDestino_alquiler(head listaalquileres)
                tipo_bici = getTipo_bici(head listaalquileres)
            in
                if id_alqui == id_alquiler then 
                    [creaAlquiler([show usuario,id_cleta,show id_alqui,"facturado",llegada,salida,tipo_bici])] ++ modificarAlquiler (tail listaalquileres) id_alquiler

                else
                    [head listaalquileres] ++ modificarAlquiler (tail listaalquileres) id_alquiler

modificarCleta :: [Bicicleta]->String->String->[Bicicleta]
modificarCleta listabicicletas id_bicicleta ubicacion=
    do  
        if null(listabicicletas) then 
            []
        else
            let 
                codigo_bicicleta = getCodigo_bicicleta(head listabicicletas)
                tipo_bicicleta = getTipo_bicicleta(head listabicicletas)
  
            in
                if id_bicicleta == codigo_bicicleta then 
                    [creaBicicleta([codigo_bicicleta,tipo_bicicleta,ubicacion])] ++ modificarCleta (tail listabicicletas) id_bicicleta ubicacion

                else
                    [head listabicicletas] ++ modificarCleta (tail listabicicletas) id_bicicleta ubicacion



-------------------------ESTADÍSTICAS------------------------------

---Estadística # 1

imprimeEstadistica::[[Integer]]->Integer->[Parqueo]->[Bicicleta]->[Usuario]->[Alquiler]->[Factura]->IO()
imprimeEstadistica estadistica contador parqueos bicicletas usuarios alquileres facturas= 
    do
        let top5= ordenaTop estadistica
        do
            if contador > 5 then
                putStr("\n")
            else
                do 
                    putStr("\n")
                    putStr (show contador++". Usuario: "++ show(head(head top5))++", Cantidad de Viajes: "++ show(last(head top5)))
                    imprimeEstadistica (tail top5) (contador+1) parqueos bicicletas usuarios alquileres facturas
        menu_estadisticas(-1,parqueos,bicicletas,usuarios,alquileres,facturas)
ordenaTop:: [[Integer]]->[[Integer]]
ordenaTop lista = 
    do            

        take 5 (sortBy (\[_,a] [_,b] -> compare b a) lista)


estadisticaInicioAux :: [Usuario] ->[Alquiler] -> [[Integer]]
estadisticaInicioAux [ ] listaalquileres = []
estadisticaInicioAux lista listaalquileres=
    
    do 
        [estadisticaInicio (head lista)  listaalquileres] ++ estadisticaInicioAux (tail lista) listaalquileres
       
estadisticaInicio :: Usuario ->[Alquiler] -> [Integer]
estadisticaInicio usuario listaalquileres=
    let 
        cedula = getCedula(usuario)
        cantidad=0
    in
        estadisticaUsuarioAux listaalquileres cedula cantidad
 

estadisticaUsuarioAux :: [Alquiler]->Integer->Integer->[Integer]
estadisticaUsuarioAux [] cedul cantidad= [cedul,cantidad]
estadisticaUsuarioAux lista cedul cantidad=
    do
        let 
            canti= (estadisticaUsuario (head lista) cedul cantidad)
        
        (estadisticaUsuarioAux (tail lista) cedul canti)

estadisticaUsuario :: Alquiler -> Integer->Integer -> Integer
estadisticaUsuario alquiler cedul cantidad=
    let 
        cedula=getUsuario_cedula(alquiler)
        estado = getEstado_alquiler(alquiler)

    in

        if cedula==cedul then 

            if estado == "activo" || estado =="facturado" then  
                
                    
                cantidad+1
            else  
                
                    
                cantidad
        else
            cantidad
          

---Estadística #2
imprimeEstadistica2::[(String,Integer)]->Integer->[Parqueo]->[Bicicleta]->[Usuario]->[Alquiler]->[Factura]->IO()
imprimeEstadistica2 estadistica contador parqueos bicicletas usuarios alquileres facturas= 
    do
        let top5= ordenaTop2 estadistica
        do
            if contador > 5 then
                putStr("\n")
            else
                do 
                    putStr("\n")
                    putStr (show contador++". Nombre Parqueo: "++ fst(head top5)++", Cantidad de Viajes(salida o destino): "++ show(snd(head top5)))
                    imprimeEstadistica2 (tail top5) (contador+1) parqueos bicicletas usuarios alquileres facturas
        menu_estadisticas(-1,parqueos,bicicletas,usuarios,alquileres,facturas)

ordenaTop2:: [(String,Integer)]->[(String,Integer)]
ordenaTop2 lista = 
    do            

        take 5 (sortBy (\(_,a) (_,b) -> compare b a) lista)


estadisticaInicio2Aux :: [Parqueo] ->[Alquiler] -> [(String,Integer)]
estadisticaInicio2Aux [ ] listaalquileres = []
estadisticaInicio2Aux lista listaalquileres=
    
    do 
       [estadisticaInicio2 (head lista)  listaalquileres] ++ estadisticaInicio2Aux (tail lista) listaalquileres
       
estadisticaInicio2 :: Parqueo ->[Alquiler] -> (String,Integer)
estadisticaInicio2 parqueo listaalquileres=
    let 
        nombre = getNombre_parqueo(parqueo)
        cantidad=0
    in
        estadisticaParqueoAux listaalquileres nombre cantidad
 

estadisticaParqueoAux :: [Alquiler]->String->Integer->(String,Integer)
estadisticaParqueoAux [] nombre cantidad= (nombre,cantidad)
estadisticaParqueoAux lista nombre cantidad=
    do
        let 
            canti= (estadisticaParqueo (head lista) nombre cantidad)
        
        (estadisticaParqueoAux (tail lista) nombre canti)

estadisticaParqueo :: Alquiler -> String->Integer -> Integer
estadisticaParqueo alquiler nombre cantidad=
    let 
        nom_sali = getSalida_alquiler(alquiler)
        nom_desti = getDestino_alquiler(alquiler)
        estado = getEstado_alquiler(alquiler)

    in

        if nombre==nom_sali || nombre == nom_desti then 

            if estado == "activo" || estado =="facturado" then  
                
                    
                cantidad+1
            else  
                
                    
                cantidad
        else
            cantidad

--Estadística # 3


imprimeEstadistica3::[(String,Integer)]->Integer->[Parqueo]->[Bicicleta]->[Usuario]->[Alquiler]->[Factura]->IO()
imprimeEstadistica3 estadistica contador parqueos bicicletas usuarios alquileres facturas= 
    do
        let top5= ordenaTop3 estadistica
        do
            if contador > 3 then
                putStr("\n")
            else
                do 
                    putStr("\n")
                    putStr (show contador++". Código de la Bicicleta: "++ fst(head top5)++", Kilometraje: "++ show(snd(head top5)))
                    imprimeEstadistica3 (tail top5) (contador+1) parqueos bicicletas usuarios alquileres facturas
        menu_estadisticas(-1,parqueos,bicicletas,usuarios,alquileres,facturas)
ordenaTop3:: [(String,Integer)]->[(String,Integer)]
ordenaTop3 lista = 
    do            

        take 3 (sortBy (\(_,a) (_,b) -> compare b a) lista)


estadisticaInicio3Aux :: [Bicicleta] ->[Factura] -> [(String,Integer)]
estadisticaInicio3Aux [ ] listafacturas = []
estadisticaInicio3Aux lista listafacturas=
    
    do 
        [estadisticaInicio3 (head lista)  listafacturas] ++ estadisticaInicio3Aux (tail lista) listafacturas
       
estadisticaInicio3 :: Bicicleta ->[Factura] -> (String,Integer)
estadisticaInicio3 bicicleta listafacturas=
    let 
        codigo=getCodigo_bicicleta(bicicleta)
        cantidad=0
    in
        estadisticaBicicletaAux listafacturas codigo cantidad
 

estadisticaBicicletaAux :: [Factura]->String->Integer->(String,Integer)
estadisticaBicicletaAux [] codigo cantidad= (codigo,cantidad)
estadisticaBicicletaAux lista codigo cantidad=
    do
        let 
            canti= (estadisticaBicicleta (head lista) codigo cantidad)
        
        (estadisticaBicicletaAux (tail lista) codigo canti)

estadisticaBicicleta :: Factura -> String->Integer -> Integer
estadisticaBicicleta factura codigo cantidad=
    let 
        cod_bici=getId_bicicleta_factura(factura)
        distancia=getDistancia_recorrido(factura)
    in

        if codigo==cod_bici then 
            distancia
        else
            0

--Estadística # 4
imprimeEstadistica4::[Integer]->[Parqueo]->[Bicicleta]->[Usuario]->[Alquiler]->[Factura]->IO()
imprimeEstadistica4 estadistica parqueos bicicletas usuarios alquileres facturas= 
    do
        putStr("\n")
        putStr ("Total de viajes: "++ show (last(estadistica))++", Total de Kilómetros: "++ show(sum(init(estadistica))) ++ ", Total Facturado: "++ show(last(estadistica)))

        menu_estadisticas(-1,parqueos,bicicletas,usuarios,alquileres,facturas)

estadisticaInicio4Aux :: [Factura] ->Integer->[Integer]
estadisticaInicio4Aux lista contador=
    
    do 
        if null(lista) then 
            [contador]
        else
            [estadisticaInicio4 (head lista)] ++ estadisticaInicio4Aux (tail lista) (contador+1)
       
estadisticaInicio4 :: Factura -> Integer
estadisticaInicio4 factura =
    let 
        kilometraje=getDistancia_recorrido(factura)
    in
        kilometraje
 