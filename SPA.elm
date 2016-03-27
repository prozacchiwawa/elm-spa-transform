...

purposeMsgMap : (PU.Model -> (SubModel, Effects action)) -> SubModel -> (SubModel, Effects action)
purposeMsgMap finishPurposeUpdate y =
    case y of
        PurposeModel p -> finishPurposeUpdate p
        mm -> (mm, Effects.none)

createMsgMap : (CW.CreateWorkout -> (SubModel, Effects action)) -> SubModel -> (SubModel, Effects action)
createMsgMap finishCreateWorkoutUpdate y =
    case y of
        CreateWorkoutModel cw -> finishCreateWorkoutUpdate cw
        mm -> (mm, Effects.none)

purposeUpdate submodelify submsgify msgMap u m =
    let finishPurposeUpdate p =
        let (pm,e) = u p in
        let newEffects = Effects.map submsgify e in
        (submodelify pm, newEffects)
    in
    let displaysWithEffects = Array.toList (Array.map (msgMap finishPurposeUpdate) m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

createUpdate submodelify submsgify msgMap u m =
    let finishCreateWorkoutUpdate p =
        let (cw,e) = u p in
        let newEffects = Effects.map submsgify e in
        (submodelify cw, newEffects)
    in
    let displaysWithEffects = Array.toList (Array.map (msgMap finishCreateWorkoutUpdate) m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

componentMapM : (Model -> (Model, Effects Action)) -> action -> EffModel Model Action -> EffModel Model Action
componentMapM updater pu effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e

handleComponentMsg : Action -> EffModel Model Action -> EffModel Model Action
handleComponentMsg action effmodel =
    case action of
        Purpose pu ->
            effmodel
                |> componentMapM (purposeUpdate PurposeModel Purpose purposeMsgMap (PU.update pu)) pu
        CreateWorkout cw ->
            effmodel
                |> componentMapM (createUpdate CreateWorkoutModel CreateWorkout createMsgMap (CW.update cw)) cw
        _ -> effmodel

...