module TowerRuntime
  ( TowerKey,
    TowerSpecialization (..),
    TowerRuntime (..),
    TowerRegistry,
    emptyTowerRegistry,
    towerKey,
    lookupTowerRuntime,
    insertTowerRuntime,
    registerTower,
    removeTower,
    upgradeTowerLevel,
    specializeTower,
    towerRegistryEntries,
  )
where

import qualified Data.Map.Strict as Map
import LI12425 (Posicao)
import MetaTypes (TowerId)

-- Grid-aligned positions are converted to integer cells so runtime metadata
-- never depends on fragile floating-point equality.
data TowerKey = TowerKey !Int !Int
  deriving (Show, Read, Eq, Ord)

data TowerSpecialization
  = EspecializacaoA
  | EspecializacaoB
  deriving (Show, Read, Eq)

data TowerRuntime = TowerRuntime
  { runtimeTowerId :: !TowerId,
    runtimeLevel :: !Int,
    runtimeSpecialization :: !(Maybe TowerSpecialization)
  }
  deriving (Show, Read, Eq)

newtype TowerRegistry = TowerRegistry (Map.Map TowerKey TowerRuntime)
  deriving (Show, Read, Eq)

emptyTowerRegistry :: TowerRegistry
emptyTowerRegistry = TowerRegistry Map.empty

towerKey :: Posicao -> TowerKey
towerKey (x, y) = TowerKey (floor x) (floor y)

lookupTowerRuntime :: Posicao -> TowerRegistry -> Maybe TowerRuntime
lookupTowerRuntime pos (TowerRegistry registry) = Map.lookup (towerKey pos) registry

insertTowerRuntime :: Posicao -> TowerRuntime -> TowerRegistry -> TowerRegistry
insertTowerRuntime pos runtime (TowerRegistry registry) =
  TowerRegistry (Map.insert (towerKey pos) runtime registry)

registerTower :: TowerId -> Posicao -> TowerRegistry -> TowerRegistry
registerTower towerId pos (TowerRegistry registry) =
  TowerRegistry $
    Map.insert
      (towerKey pos)
      (TowerRuntime towerId 1 Nothing)
      registry

removeTower :: Posicao -> TowerRegistry -> TowerRegistry
removeTower pos (TowerRegistry registry) =
  TowerRegistry (Map.delete (towerKey pos) registry)

upgradeTowerLevel :: Int -> Posicao -> TowerRegistry -> TowerRegistry
upgradeTowerLevel maxLevel pos (TowerRegistry registry) =
  TowerRegistry $
    Map.adjust
      (\runtime -> runtime {runtimeLevel = min maxLevel (runtimeLevel runtime + 1)})
      (towerKey pos)
      registry

specializeTower :: TowerSpecialization -> Posicao -> TowerRegistry -> TowerRegistry
specializeTower specialization pos (TowerRegistry registry) =
  TowerRegistry $
    Map.adjust
      (\runtime -> runtime {runtimeSpecialization = Just specialization})
      (towerKey pos)
      registry

towerRegistryEntries :: TowerRegistry -> [(TowerKey, TowerRuntime)]
towerRegistryEntries (TowerRegistry registry) = Map.toList registry
