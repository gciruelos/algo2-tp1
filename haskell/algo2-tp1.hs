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

data DCNET = NuevaRed (Set Computadora) (Set Conexion) | IngresarPaquete DCNET Computadora Paquete | SiguienteSegundo DCNET deriving(Show)


----------TESTING
interFaces = Ag 1 $ Ag 2 Vacio

compu1 = CrearComputadora 1 interFaces
compu2 = CrearComputadora 2 interFaces
compu3 = CrearComputadora 3 interFaces

lasConexiones = Ag  (CrearConexion (compu1,1) (compu2,1))
                 $ Ag (CrearConexion (compu2,2) (compu3,1))
                   $ Ag (CrearConexion (compu3,2) (compu1,2)) Vacio

-------



-------------------------------------------------------------------------------
----------- COMPUTADORA -------------------------------------------------------
-------------------------------------------------------------------------------
-- observadores
ip :: Computadora -> Int
ip (CrearComputadora i _) = i


tieneInterfaz :: Computadora -> Interfaz -> Bool
tieneInterfaz (CrearComputadora _ s) i = i `en` s


interfaces (CrearComputadora _ s) = s
instance Eq Computadora where
  compu == compu' = ip compu == ip compu' && interfaces compu == interfaces compu'

instance Show Computadora where
  show (CrearComputadora ip _) = "Computadora IP: "++(show ip)


-- otras operaciones
mismaIP c1 c2 = (ip c1) == (ip c2)

-------------------------------------------------------------------------------
----------- CONEXION ----------------------------------------------------------
-------------------------------------------------------------------------------
-- observadores
computadorasQueConecta :: Conexion -> Set Computadora
computadorasQueConecta (CrearConexion (p1, _) (p2, _)) = Ag p1 (Ag p2 Vacio)

instance Eq Conexion where
  x1 == x2 = (computadorasQueConecta x1) == (computadorasQueConecta x2)

-- otras operaciones
conectaA :: Conexion -> Computadora -> Bool
conectaA conexion compu = compu `en` (computadorasQueConecta conexion)

laOtraCompu :: Conexion -> Computadora ->  Computadora
laOtraCompu conexion compu = dameUno ((computadorasQueConecta conexion) -: compu)


estanConectadas :: Conexion -> Computadora -> Computadora -> Bool
estanConectadas conexion c1 c2 = conectaA conexion c1 && conectaA conexion c2

vecinos :: Set Conexion -> Computadora -> Set Computadora
vecinos s compu = if esVacio s then Vacio else
                      if conectaA (dameUno s) compu
                        then Ag (laOtraCompu (dameUno s) compu) (vecinos (sinUno s) compu)
                        else vecinos (sinUno s) compu

dameConexion :: Set Conexion -> Computadora -> Computadora -> Conexion
dameConexion s c1 c2 = if estanConectadas (dameUno s) c1 c2 then dameUno s
                           else dameConexion (sinUno s) c1 c2

-------------------------------------------------------------------------------
----------- AUXILIARES CAMINOS-------------------------------------------------
-------------------------------------------------------------------------------
agregarAlPrincipio :: Eq a => a -> Set [a] -> Set [a]
agregarAlPrincipio a conj = if esVacio conj then Vacio else Ag (a:(dameUno conj)) (sinUno conj)

caminosDeLargoN :: Set Conexion -> Computadora -> Int -> Set [Computadora]
caminosDeLargoN s origen n = 
        if n == 0 then Ag [origen] Vacio
              else if esVacio (vecinos s origen) then Vacio
                else agregarAlPrincipio origen ((caminosDeLargoN s (dameUno(vecinos s origen)) (n-1))    `union`
                      caminosDeLargoN (s-:(dameConexion s origen (dameUno (vecinos s origen)))) origen n)

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

caminoMasCortoDeLargoN :: Set Conexion -> Computadora -> Computadora -> Int -> [Computadora]
caminoMasCortoDeLargoN conexiones origen destino n =
  if not $ esVacio (filtrarCaminos (caminosDeLargoN conexiones origen n) origen destino)
    then dameUno (filtrarCaminos (caminosDeLargoN conexiones origen n) origen destino) 
    else caminoMasCortoDeLargoN conexiones origen destino (n+1)

caminoMasCorto :: Set Conexion -> Computadora -> Computadora -> [Computadora]
caminoMasCorto conexiones origen destino = caminoMasCortoDeLargoN conexiones origen destino 0

-------------------------------------------------------------------------------
----------- DCNET -------------------------------------------------------------
-------------------------------------------------------------------------------
agregarMuchos :: Eq a => Set a -> PQ a -> PQ a
agregarMuchos s pq = if esVacio s then pq else agregarMuchos (sinUno s) (Enqueue (dameUno s) pq)
-- observadores
paquetesEnPC :: DCNET -> Computadora -> PQ Paquete
paquetesEnPC (NuevaRed _ _) _ = Empty 
paquetesEnPC (IngresarPaquete red compu' paq) compu = if compu==compu' then Enqueue paq (paquetesEnPC red compu) else paquetesEnPC red compu
paquetesEnPC (SiguienteSegundo red) compu = agregarMuchos (paquetesRecibidos red compu) (desencolar (paquetesEnPC red compu))

topologia :: DCNET -> Set Conexion
topologia (NuevaRed _ conexiones) = conexiones
topologia (IngresarPaquete red _ _ ) = topologia red
topologia (SiguienteSegundo red) = topologia red

-- otras operaciones
--
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

cantDeElementos :: PQ a -> Int
cantDeElementos pq = if vacia pq then 0 else 1+(cantDeElementos(desencolar(pq)))

cantPaquetesEnPC :: DCNET -> Computadora -> Int
cantPaquetesEnPC red compu = cantDeElementos (paquetesEnPC red compu)


-- TODO -> funciones que faltan implementar:
-- recorridoPaquete :: DCNET -> Paquete -> [Conexion] (facil teniendo caminoMasCorto, pero hay cuestion sobre como identificar paquetes distintos)
-- computadoraQueEnvioMasPaquetes :: DCNET -> Computadora (no pense como implementarla)


