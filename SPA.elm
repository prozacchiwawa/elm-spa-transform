...

componentUpdate msgMap u action m =
    let displaysWithEffects = Array.toList (Array.map (msgMap u action) m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

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
    effmodel
        |> componentMapM action (componentUpdate purposeMsgMap PU.update)
        |> componentMapM action (componentUpdate createMsgMap CW.update)

...