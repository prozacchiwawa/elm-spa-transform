import Container exposing (Container, componentMapM, componentUpdate, msgMapper)

...

componentToModel : Container Model SubModel
componentToModel =
    Container.create (\m -> m.display) (\d m -> { m | display = d })

purposeMsgMap : Action -> SubModel -> (SubModel, Effects Action)
purposeMsgMap action y =
    let puUpdate = msgMapper PU.update Purpose PurposeModel in
    case (action,y) of
        (Purpose a, PurposeModel p) -> puUpdate a p
        (_,mm) -> (mm, Effects.none)

createMsgMap : Action -> SubModel -> (SubModel, Effects Action)
createMsgMap action y =
    let cwUpdate = msgMapper CW.update CreateWorkout CreateWorkoutModel in
    case (action,y) of
        (CreateWorkout a, CreateWorkoutModel p) -> cwUpdate a p
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