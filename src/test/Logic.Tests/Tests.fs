module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Requests on 2 requests overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }
  ]

[<Tests>]
let overlapWithAnyTests =
  testList "Overlap with any tests" [
    test "Test if one date is overlap" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }
      let requests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        }
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 4); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 5); HalfDay = PM }
        }
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 6); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 7); HalfDay = PM }
        }
      ] 
      Expect.isTrue(Logic.overlapsWithAnyRequest requests request) "One date is overlap"
    }
    test "Test if no date is overlap" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 09, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 09, 2); HalfDay = PM }
      }
      let requests = [
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        }
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 4); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 5); HalfDay = PM }
        }
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 6); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 7); HalfDay = PM }
        }
      ] 
      Expect.isFalse(Logic.overlapsWithAnyRequest requests request) "One date is overlap"
    }
  ]


[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "Create a request in the past" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request shouldn't have been created"
    }

    test "Create overlap request" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 18); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 23); HalfDay = PM } }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 20); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 25); HalfDay = PM } }

      Given [ RequestCreated request1 ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request2)
      |> Then (Error "Overlapping request") "The request shouldn't have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    test "Try to validate a not created request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be validated") "The request shouldn't have been validated"
    }

    test "Try to validate a request already validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be validated") "The request shouldn't have been validated"
    }
    test "A request pending for cancelation is validated (not canceled)" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }
      
      Given [ RequestAskedForCancel request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let cancelationTests =
  testList "Cancelation tests" [
    test "A request is canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }
      
      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled"
    }

    test "Try to cancel a not created request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be canceled") "The request shouldn't have been canceled"
    }

    test "Try to cancel a request already canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestCanceled request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Request already canceled") "The request shouldn't have been canceled"
    }

    test "Cancel a validated request in past as employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 01); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 01); HalfDay = PM } }
      let user = Employee "jdoe"
      Given [ RequestValidated request ]
      |> ConnectedAs user
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestAskedForCancel request]) "The request should have been asked for cancel"
    }

    test "Cancel a validated request as employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 04, 01); HalfDay = AM }
        End = { Date = DateTime(2020, 04, 01); HalfDay = PM } }
      let user = Employee "jdoe"
      Given [ RequestValidated request ]
      |> ConnectedAs user
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled"
    }

    test "A request pending for cancelation is canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }
      
      Given [ RequestAskedForCancel request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled"
    }
  ]

[<Tests>]
let refusationTests =
  testList "Refusation tests" [
    test "A request is refused" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (RefuseRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestRefused request]) "The request should have been refused"
    }

    test "Try to refuse a not created request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs Manager
      |> When (RefuseRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be refused") "The request shouldn't have been refused"
    }

    test "Try to refuse a request already refused" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ RequestRefused request ]
      |> ConnectedAs Manager
      |> When (RefuseRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be refused") "The request shouldn't have been refused"
    }
  ]