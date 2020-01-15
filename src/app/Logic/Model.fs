namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCanceled of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestAskedForCancel of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCanceled request -> request 
        | RequestRefused request -> request 
        | RequestAskedForCancel request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    
    let getCurrentDate () =
        DateTime.Now
        
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | PendingCancelation of TimeOffRequest
        | Validated of TimeOffRequest
        | Canceled of TimeOffRequest 
        | Refused of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | PendingCancelation request
            | Validated request -> request
            | Canceled request -> request
            | Refused request -> request 
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | PendingCancelation _ -> true
            | Validated _ -> true
            | Canceled _ -> false
            | Refused _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceled request -> Canceled request
        | RequestRefused request -> Refused request 
        |RequestAskedForCancel request -> PendingCancelation request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =
        (request1.Start.Date <= request2.End.Date) && (request1.End.Date >= request2.Start.Date)

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        otherRequests |> Seq.exists (fun otherRequest -> overlapsWith request otherRequest)

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= getCurrentDate() then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | PendingCancelation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelRequestFromUser requestState =
                match requestState with
                | PendingValidation request ->
                    Ok [RequestCanceled request]
                | Validated request ->
                    if requestState.Request.Start.Date > getCurrentDate() then
                        Ok [RequestCanceled request]
                    else
                        Ok [RequestAskedForCancel request]
                | _ ->
                    Error "Request cannot be canceled"

    let cancelRequest requestState =
            match requestState with
            | PendingValidation request ->
                Ok [RequestCanceled request]
            | Validated request ->
                Ok [RequestCanceled request]
            | Canceled _->
                Error "Request already canceled"
            | PendingCancelation request ->
                Ok [RequestCanceled request]
            | _ ->
                Error "Request cannot be canceled"

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refused"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_, requestId) -> 
                    let requestState = defaultArg(userRequests.TryFind requestId) NotCreated
                    if user <> Manager then
                        cancelRequestFromUser requestState
                    else
                        cancelRequest requestState
            | RefuseRequest (_, requestId) -> 
                if user <> Manager then
                    Error "Unauthorized"
                else 
                    let requestState = defaultArg(userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState
    
    // Ensemble des congés attribués depuis le début de l'année
    // = Tous les congés   
    let attributedDays =
        float (getCurrentDate().Month - 1) * 2.5
    
    // TODO : Manque ici la fonction donnant le report du solde de l'année passée
    
    // Ensemble des congés effectifs
    // = Les congés demandés du début d'année jusqu'à la date demandée
    let effectiveUserRequestsUntilDate (requests: TimeOffRequest seq) (userId: UserId) (date: DateTime) =
        requests
            |> Seq.filter (fun request -> request.UserId = userId)
            |> Seq.filter (fun request -> request.Start.Date.Year = getCurrentDate().Year)
            |> Seq.filter (fun request -> request.Start.Date <= date)
    
    // Ensemble des congés prévus
    // = Les congés prévus de la date demandée jusqu'à la fin d'année
    let plannedUserRequestsFromDate (requests: TimeOffRequest seq) (userId: UserId) (date: DateTime) =
        requests
            |> Seq.filter (fun request -> request.UserId = userId)
            |> Seq.filter (fun request -> request.Start.Date.Year = getCurrentDate().Year)
            |> Seq.filter (fun request -> request.Start.Date > date)
    
    // Solde diponible
    // = Attribués - (effectifs + prévus)
    // TODO : La fonction n'est pas bonne car soustrait le total au nombre de congés demandés dans l'année
    // TODO : Elle doit le soustraire au nombre de jours de congés de chacune de ces demandes
    let availableDays (requests: TimeOffRequest seq) (userId: UserId) =
        attributedDays
            // TODO : + reportedDays
            - float (effectiveUserRequestsUntilDate requests userId (getCurrentDate()) |> Seq.length)
            - float (plannedUserRequestsFromDate requests userId (getCurrentDate()) |> Seq.length)