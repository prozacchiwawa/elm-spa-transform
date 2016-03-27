...

componentUpdate msgMap u m =
    let displaysWithEffects = Array.toList (Array.map (msgMap u) m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

componentMapM : (Model -> (Model, Effects Action)) -> action -> EffModel Model Action -> EffModel Model Action
componentMapM updater pu effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e

purposeMsgMap finishPurposeUpdate y =
    let effectMap (m,e) =
        (PurposeModel m, Effects.map Purpose e)
    in
    case y of
        PurposeModel p -> finishPurposeUpdate p |> effectMap
        mm -> (mm, Effects.none)

createMsgMap finishCreateWorkoutUpdate y =
    let effectMap (m,e) =
        (CreateWorkoutModel m, Effects.map CreateWorkout e)
    in
    case y of
        CreateWorkoutModel cw -> finishCreateWorkoutUpdate cw |> effectMap
        mm -> (mm, Effects.none)

handleComponentMsg : Action -> EffModel Model Action -> EffModel Model Action
handleComponentMsg action effmodel =
    case action of
        Purpose pu ->
            effmodel
                |> componentMapM (componentUpdate purposeMsgMap (PU.update pu)) pu
        CreateWorkout cw ->
            effmodel
                |> componentMapM (componentUpdate createMsgMap (CW.update cw)) cw
        _ -> effmodel

...