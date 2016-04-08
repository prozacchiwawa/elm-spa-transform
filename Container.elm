module Container where

import Array exposing (Array(..))
import Effects exposing (Effects(..))
import Signals.EffModel as EF exposing (EffModel, eff)

type alias Container model submodel = {
        getDisplays : model -> Array submodel
    ,   updateDisplays : Array submodel -> model -> model
    }

create : (model -> Array submodel) -> (Array submodel -> model -> model) -> Container model submodel
create getDisplays updateDisplays =
    { getDisplays = getDisplays, updateDisplays = updateDisplays }

msgMapper : (action -> model -> (model, Effects action)) -> (action -> actionA) -> (model -> modelA) -> action -> model -> (modelA, Effects actionA)
msgMapper update liftAction liftModel action model =
    let (m,e) = update action model in
    (liftModel m, Effects.map liftAction e)

componentUpdate componentToModel msgMap action m =
    let displaysWithEffects = Array.toList (Array.map (msgMap action) (componentToModel.getDisplays m))
    in
    (componentToModel.updateDisplays (Array.fromList (List.map fst displaysWithEffects)) m, Effects.batch (List.map snd displaysWithEffects))

componentMapM action updater effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater action y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e
