import Set
import PriorityQueue


type Interfaz = Int

data Paquete = P Int Int Int deriving(Eq, Show)
prioridad (P p _ _) = p
desde (P _ d _) = d
hasta (P _ _ h) = h

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
conectaA :: Conexion -> Computadora -> Bool
conectaA (CrearConexion (p1, _) (p2, _)) compu = p1 == compu || p2 == compu

-- no me gusta, me gustaria que esto fuera un observador. TODO
laOtraCompu :: Conexion -> Computadora -> Computadora
laOtraCompu (CrearConexion (c1, _) (c2, _)) compu = if c1 == compu then c2 else c1

instance Eq Conexion where
  CrearConexion (c1, i1) (c2, i2) == CrearConexion (c3, i3) (c4, i4) = 
                (c1 == c3 && i1 == i3 && c2 == c4 && i2 == i4) ||
                (c1 == c4 && i1 == i4 && c2 == c3 && i2 == i3)

-- otras operaciones
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

paquetesEnPC :: DCNET -> Computadora -> PQ Paquete
paquetesEnPC (NuevaRed _ _) _ = Empty 
paquetesEnPC (IngresarPaquete red compu' paq) compu = if compu==compu' then Enqueue paq (paquetesEnPC red compu) else paquetesEnPC red compu
paquetesEnPC (SiguienteSegundo red) compu = agregarMuchos (paquetesRecibidos red compu) (desencolar (paquetesEnPC red compu))

paquetesRecibidos :: DCNET -> Computadora -> Set Paquete
paquetesRecibidos red compu = Vacio
-- paquetesRecibidos red compu = (Ag compu (vecinos red compu))
-- TODO: falta implementar bien esta funcion. Hay que filtrar ^ este conjunto
-- de tal manera que solo queden las computadoras que mandan un paquete a compu,
-- y despues dada ese conjunto c de computadoras, hay que devolver 
-- map (proxima.paquetes) c



-- TODO -> funciones que faltan implementar:
-- recorridoPaquete :: DCNET -> Paquete -> [Conexion] (facil teniendo caminoMasCorto, pero hay cuestion sobre como identificar paquetes distintos)
-- computadoraQueEnvioMasPaquetes :: DCNET -> Computadora (no pense como implementarla
-- #paquetesEnPC :: DCNET -> Computadora -> Nat (facil teniendo paquetesEnPC)


