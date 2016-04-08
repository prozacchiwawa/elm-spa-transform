module Container where

import Array exposing (Array(..))
import Effects
import Signals.EffModel as EF exposing (eff)

type alias Container model submodel = {
        getDisplays : model -> Array submodel
    ,   updateDisplays : Array submodel -> model -> model
    }

create : (model -> Array submodel) -> (Array submodel -> model -> model) -> Container model submodel
create getDisplays updateDisplays =
    { getDisplays = getDisplays, updateDisplays = updateDisplays }

componentUpdate componentToModel msgMap u action m =
    let displaysWithEffects = Array.toList (Array.map (msgMap action) (componentToModel.getDisplays m))
    in
    (componentToModel.updateDisplays (Array.fromList (List.map fst displaysWithEffects)) m, Effects.batch (List.map snd displaysWithEffects))

componentMapM action updater effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater action y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e
