import Container exposing (Container, componentMapM, componentUpdate)

...

componentToModel : Container Model SubModel
componentToModel =
    Container.create (\m -> m.display) (\d m -> { m | display = d })

purposeMsgMap action y =
    let effectMap (m,e) =
        (PurposeModel m, Effects.map Purpose e)
    in
    case (action,y) of
        (Purpose a, PurposeModel p) -> PU.update a p |> effectMap
        (_,mm) -> (mm, Effects.none)

createMsgMap action y =
    let effectMap (m,e) =
        (CreateWorkoutModel m, Effects.map CreateWorkout e)
    in
    case (action,y) of
        (CreateWorkout a, CreateWorkoutModel cw) -> CW.update a cw |> effectMap
        (_,mm) -> (mm, Effects.none)

handleComponentMsg : Action -> EffModel Model Action -> EffModel Model Action
handleComponentMsg action effmodel =
    let updateFn update effmodel =
        componentMapM action update effmodel
    in
    List.foldr updateFn effmodel [
            componentUpdate componentToModel purposeMsgMap
        ,   componentUpdate componentToModel createMsgMap
        ]

...