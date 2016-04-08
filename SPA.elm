...

componentToModel = { getDisplays = \m -> m.display, updateDisplays = \d m -> { m | display = d } }

componentUpdate componentToModel msgMap u action m =
    let displaysWithEffects = Array.toList (Array.map (msgMap u action) (componentToModel.getDisplays m))
    in
    (componentToModel.updateDisplays (Array.fromList (List.map fst displaysWithEffects)) m, Effects.batch (List.map snd displaysWithEffects))

componentMapM : Action -> (Action -> Model -> (Model, Effects Action)) -> EffModel Model Action -> EffModel Model Action
componentMapM action updater effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater action y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e

purposeMsgMap finishPurposeUpdate action y =
    let effectMap (m,e) =
        (PurposeModel m, Effects.map Purpose e)
    in
    case (action,y) of
        (Purpose a, PurposeModel p) -> finishPurposeUpdate a p |> effectMap
        (_,mm) -> (mm, Effects.none)

createMsgMap finishCreateWorkoutUpdate action y =
    let effectMap (m,e) =
        (CreateWorkoutModel m, Effects.map CreateWorkout e)
    in
    case (action,y) of
        (CreateWorkout a, CreateWorkoutModel cw) -> finishCreateWorkoutUpdate a cw |> effectMap
        (_,mm) -> (mm, Effects.none)

handleComponentMsg : Action -> EffModel Model Action -> EffModel Model Action
handleComponentMsg action effmodel =
    let updateFn update effmodel =
        componentMapM action update effmodel
    in
    List.foldr updateFn effmodel [
            componentUpdate componentToModel purposeMsgMap PU.update
        ,   componentUpdate componentToModel createMsgMap CW.update
        ]

...