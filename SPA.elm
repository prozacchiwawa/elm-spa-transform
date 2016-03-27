...

purposeUpdate : (PU.Model -> (PU.Model, Effects PU.Action)) -> Model -> (Model, Effects Action)
purposeUpdate u m =
    let finishPurposeUpdate p =
        let (pm,e) = u p in
        let newEffects = Effects.map Purpose e in
        (PurposeModel pm, newEffects)
    in
    let updatePurpose y =
        case y of
            PurposeModel p -> finishPurposeUpdate p
            mm -> (mm, Effects.none)
    in
    let displaysWithEffects = Array.toList (Array.map updatePurpose m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

createUpdate : (CW.CreateWorkout -> (CW.CreateWorkout, Effects CW.InputAction)) -> Model -> (Model, Effects Action)
createUpdate u m =
    let finishCreateWorkoutUpdate p =
        let (cw,e) = u p in
        let newEffects = Effects.map CreateWorkout e in
        (CreateWorkoutModel cw, newEffects)
    in
    let updateCreateWorkout y =
        case y of
            CreateWorkoutModel cw -> finishCreateWorkoutUpdate cw
            mm -> (mm, Effects.none)
    in
    let displaysWithEffects = Array.toList (Array.map updateCreateWorkout m.display)
    in
    ({ m | display = Array.fromList (List.map fst displaysWithEffects) }, Effects.batch (List.map snd displaysWithEffects))

componentMapM : (Model -> (Model, Effects Action)) -> action -> EffModel Model Action -> EffModel Model Action
componentMapM updater pu effmodel =
    let y = EF.get effmodel in
    let (m, e) = updater y in
    effmodel
        |> EF.map (\_ -> m)
        |> eff e

createMapM : CW.InputAction -> EffModel Model Action -> EffModel Model Action
createMapM cw effmodel =
    let y = EF.get effmodel in
    let (m, e) = (createUpdate (CW.update cw) y) in
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
                |> createMapM cw
        _ -> effmodel

...