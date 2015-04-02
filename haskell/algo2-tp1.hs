import Set
import PriorityQueue


type Interfaz = Int

data Paquete = P Int Computadora Computadora deriving(Eq, Show)
prioridadPaq (P p _ _) = p
origenPaq (P _ d _) = d
destinoPaq (P _ _ h) = h

instance Ord Paquete where
  compare (P a _ _) (P a' _ _) = compare a a'

data Computadora = CrearComputadora Int (Set Interfaz)

data Conexion = CrearConexion (Computadora, Interfaz) (Computadora, Interfaz) deriving(Show)

data Topologia = Vacia | AgregarComputadora Topologia Computadora | AgregarConexion Topologia Conexion deriving(Show)

data DCNET = NuevaRed Topologia | IngresarPaquete DCNET Computadora Paquete | SiguienteSegundo DCNET deriving(Show)


---------- TESTING ------------------------------------------------------------
--interFaces = Ag 1 $ Ag 2 Vacio

--compu1 = CrearComputadora 1 interFaces
--compu2 = CrearComputadora 2 interFaces
--compu3 = CrearComputadora 3 interFaces

--lasConexiones = Ag  (CrearConexion (compu1,1) (compu2,1))
--                 $ Ag (CrearConexion (compu2,2) (compu3,1))
--                   $ Ag (CrearConexion (compu3,2) (compu1,2)) Vacio
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
----------- AUXILIARES --------------------------------------------------------
-------------------------------------------------------------------------------
agregarAlPrincipio :: Eq a => a -> Set [a] -> Set [a]
agregarAlPrincipio a conj = if esVacio conj then Vacio else Ag (a:(dameUno conj)) (sinUno conj)

agregarMuchos :: Eq a => Set a -> PQ a -> PQ a
agregarMuchos s pq = if esVacio s then pq else agregarMuchos (sinUno s) (Enqueue (dameUno s) pq)

cantDeElementos :: Ord a => PQ a -> Int
cantDeElementos pq = if vacia pq then 0 else 1+(cantDeElementos(desencolar(pq)))


-------------------------------------------------------------------------------
----------- COMPUTADORA -------------------------------------------------------
-------------------------------------------------------------------------------
-- observadores
ip :: Computadora -> Int
ip (CrearComputadora i _) = i


tieneInterfaz :: Computadora -> Interfaz -> Bool
tieneInterfaz (CrearComputadora _ s) i = i `en` s

-- otras operaciones
mismaIP c1 c2 = (ip c1) == (ip c2)

-- haskell
interfaces (CrearComputadora _ s) = s
instance Eq Computadora where
  compu == compu' = ip compu == ip compu' && interfaces compu == interfaces compu'

instance Show Computadora where
  show (CrearComputadora ip _) = "Computadora IP: "++(show ip)


-------------------------------------------------------------------------------
----------- CONEXION ----------------------------------------------------------
-------------------------------------------------------------------------------
-- observadores
computadorasQueConecta :: Conexion -> Set Computadora
computadorasQueConecta (CrearConexion (p1, _) (p2, _)) = Ag p1 (Ag p2 Vacio)

-- otras operaciones
conectaA :: Conexion -> Computadora -> Bool
conectaA conexion compu = compu `en` (computadorasQueConecta conexion)

laOtraCompu :: Conexion -> Computadora ->  Computadora
laOtraCompu conexion compu = dameUno ((computadorasQueConecta conexion) -: compu)


estanConectadas :: Conexion -> Computadora -> Computadora -> Bool
estanConectadas conexion c1 c2 = conectaA conexion c1 && conectaA conexion c2

-- haskell
instance Eq Conexion where
  x1 == x2 = (computadorasQueConecta x1) == (computadorasQueConecta x2)


-------------------------------------------------------------------------------
----------- TOPOLOGIA ---------------------------------------------------------
-------------------------------------------------------------------------------

-- observadores
computadoras :: Topologia -> Set Computadora
computadoras Vacia = Vacio
computadoras (AgregarComputadora top compu) = Ag compu (computadoras top)
computadoras (AgregarConexion top conx) = (computadoras top)

conexiones :: Topologia -> Set Conexion
conexiones Vacia = Vacio
conexiones (AgregarComputadora top compu) = (conexiones top)
conexiones (AgregarConexion top conx) = Ag conx (conexiones top)

quitarConexion :: Topologia -> Conexion -> Topologia
quitarConexion Vacia conexion = Vacia
quitarConexion (AgregarComputadora top compu) conexion = quitarConexion top conexion
quitarConexion (AgregarConexion top conexion') conexion  = if conexion == conexion' then quitarConexion top conexion else AgregarConexion (quitarConexion top conexion) conexion'

-------------------------------------------------------------------------------
----------- AUXILIARES CAMINOS-------------------------------------------------
-------------------------------------------------------------------------------
vecinos :: Topologia -> Computadora -> Set Computadora
vecinos s compu = 
  if esVacio (conexiones s) then Vacio else
    if conectaA (dameUno (conexiones s)) compu
      then Ag (laOtraCompu (dameUno (conexiones s)) compu)
                (vecinos (quitarConexion s (dameUno (conexiones s))) compu)
      else vecinos (quitarConexion s (dameUno (conexiones s))) compu

dameConexion :: Topologia -> Computadora -> Computadora -> Conexion
dameConexion s c1 c2 = if estanConectadas (dameUno (conexiones s)) c1 c2
                         then dameUno (conexiones s)
                         else dameConexion (quitarConexion s (dameUno (conexiones s))) c1 c2

caminosDeLargoN :: Topologia -> Computadora -> Int -> Set [Computadora]
caminosDeLargoN s origen n = 
        if n == 0 then Ag [origen] Vacio
              else if esVacio (vecinos s origen) then Vacio
                else agregarAlPrincipio origen
                      ((caminosDeLargoN s (dameUno(vecinos s origen)) (n-1))    `union`
                      caminosDeLargoN (quitarConexion s (dameConexion s origen (dameUno (vecinos s origen)))) origen n)
--
esCamino :: [Computadora] -> Computadora -> Computadora -> Bool
esCamino camino origen destino = length camino > 0 && -- es Luego
                                 origen == head camino &&
                                 destino == last camino

filtrarCaminos :: Set [Computadora] -> Computadora -> Computadora -> Set [Computadora]
filtrarCaminos caminos origen destino =
    if esVacio caminos then Vacio
      else if esCamino (dameUno caminos) origen destino
        then Ag (dameUno caminos) (filtrarCaminos (sinUno caminos) origen destino)
        else filtrarCaminos (sinUno caminos) origen destino

caminoMasCortoDeLargoN :: Topologia -> Computadora -> Computadora -> Int -> [Computadora]
caminoMasCortoDeLargoN conexiones origen destino n =
  if not $ esVacio (filtrarCaminos (caminosDeLargoN conexiones origen n) origen destino)
    then dameUno (filtrarCaminos (caminosDeLargoN conexiones origen n) origen destino) 
    else caminoMasCortoDeLargoN conexiones origen destino (n+1)

caminoMasCorto :: Topologia -> Computadora -> Computadora -> [Computadora]
caminoMasCorto top origen destino = caminoMasCortoDeLargoN top origen destino 0




-------------------------------------------------------------------------------
----------- DCNET -------------------------------------------------------------
-------------------------------------------------------------------------------
-- observadores

paquetesEnPC :: DCNET -> Computadora -> PQ Paquete
paquetesEnPC (NuevaRed _) _ = Empty 
paquetesEnPC (IngresarPaquete red compu' paq) compu = if compu==compu' then Enqueue paq (paquetesEnPC red compu) else paquetesEnPC red compu
paquetesEnPC (SiguienteSegundo red) compu = agregarMuchos (paquetesRecibidos red compu) (desencolar (paquetesEnPC red compu))

topologia :: DCNET -> Topologia
topologia (NuevaRed top) = top
topologia (IngresarPaquete red _ _ ) = topologia red
topologia (SiguienteSegundo red) = topologia red

-- otras operaciones
-- La mayoria en realidad no son otras opreaciones propiamente dichas
siguienteCompu :: DCNET -> Computadora -> Computadora -> Computadora
siguienteCompu red actual destino =
       if length (caminoMasCorto (topologia red) actual destino) == 1
        then actual
        else head (tail (caminoMasCorto (topologia red) actual destino))

filtrarVaAPC :: DCNET -> Set Computadora -> Computadora -> Set Paquete
filtrarVaAPC red compus compu =
     if esVacio compus then Vacio
       else if siguienteCompu red (dameUno compus)
                  (destinoPaq (proxima (paquetesEnPC red (dameUno compus)))) ==  compu
              then Ag (proxima (paquetesEnPC red (dameUno compus))) (filtrarVaAPC red (sinUno compus) compu) 
              else filtrarVaAPC red (sinUno compus) compu

sacarSiEsDestino :: Set Paquete -> Computadora -> Set Paquete
sacarSiEsDestino paqs compu =
     if esVacio paqs
      then Vacio
      else if destinoPaq (dameUno paqs) == compu
             then sacarSiEsDestino (sinUno paqs) compu
             else Ag (dameUno paqs) (sacarSiEsDestino (sinUno paqs) compu)

-- paquetesRecibidos , los paquetes que va a recibir la computadora cuando pase el proximo segundo
paquetesRecibidos :: DCNET -> Computadora -> Set Paquete
paquetesRecibidos red compu = sacarSiEsDestino (filtrarVaAPC red (Ag compu (vecinos (topologia red) compu)) compu ) compu
--

cantPaquetesEnPC :: DCNET -> Computadora -> Int
cantPaquetesEnPC red compu = cantDeElementos (paquetesEnPC red compu)


-- TODO -> funciones que faltan implementar:
-- recorridoPaquete :: DCNET -> Paquete -> [Conexion] (facil teniendo caminoMasCorto, pero hay cuestion sobre como identificar paquetes distintos)
-- computadoraQueEnvioMasPaquetes :: DCNET -> Computadora (no pense como implementarla)

