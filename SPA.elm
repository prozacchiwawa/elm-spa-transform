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

purposeUpdate : (PU.Model -> (PU.Model, Effects PU.Action)) -> Model -> (Model, Effects Action)
purposeUpdate u m =
    let finishPurposeUpdate p =
        let (pm,e) = u p in
        let newEffects = Effects.map Purpose e in
        (PurposeModel pm, newEffects)
    in
    let displaysWithEffects = Array.toList (Array.map (purposeMsgMap finishPurposeUpdate) m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

createUpdate : (CW.CreateWorkout -> (CW.CreateWorkout, Effects CW.InputAction)) -> Model -> (Model, Effects Action)
createUpdate u m =
    let finishCreateWorkoutUpdate p =
        let (cw,e) = u p in
        let newEffects = Effects.map CreateWorkout e in
        (CreateWorkoutModel cw, newEffects)
    in
    let displaysWithEffects = Array.toList (Array.map (createMsgMap finishCreateWorkoutUpdate) m.display)
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
                |> componentMapM (purposeUpdate (PU.update pu)) pu
        CreateWorkout cw ->
            effmodel
                |> componentMapM (createUpdate (CW.update cw)) cw
        _ -> effmodel

...