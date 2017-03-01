module Bank 
 
open System

type Command = Deposit of int | Withdraw of int
type Query = Deposits | Withdraws | OrderByDate | OrderByAmount | Last of int
type Operation = Deposited of int | Withdrawed of int
and Transaction = DateTime * Operation
and Account = { Balance: int; Transactions: Transaction list }

let operate account command date =
    match command with 
    | Deposit(x) -> date, Deposited x
    | Withdraw(x) -> date, Withdrawed x
    
let apply account transaction =
    let impact = 
        match transaction with 
        | _, Deposited(x)  -> +x
        | _, Withdrawed(x) -> -x

    { Balance = account.Balance + impact
      Transactions = transaction :: account.Transactions }

let reduce query transactions  =
    let isDeposit = function | Deposited(_) -> true | _ -> false
    let isWithdrawal = function | Withdrawed(_) -> true | _ -> false
    let getAmount = function | Deposited(x) -> x | Withdrawed(x) -> x

    transactions |>
    match query with
    | Deposits ->  List.filter (snd >> isDeposit)
    | Withdraws -> List.filter (snd >> isWithdrawal)
    | OrderByDate -> List.sortBy fst
    | OrderByAmount -> List.sortBy (snd >> getAmount)
    | Last(count) -> Seq.take count >> List.ofSeq


/// Tests

open Xunit
open Helpers

type ``Given an account``() = 
    let now = DateTime.UtcNow
    let date0 = now
    let date1 = now.AddSeconds(1.)
    let date2 = now.AddSeconds(2.)
    let date3 = now.AddSeconds(3.)
    let date4 = now.AddSeconds(4.)

    let Given events = Seq.fold apply { Balance = 0; Transactions = []} (List.rev events)
    let When date command state = (operate state command date, state)
    let Then expected (result, state)  = apply state result |> ``should equal`` expected

    [<Fact>]
    let ``with an empty account when 100 deposit then the account should be 100``() =
        Given []
        |> When date0 (Deposit 100) 
        |> Then { Balance = 100; 
                    Transactions = [date0, Deposited(100)] }

    [<Fact>]
    let ``with 1100 deposits when 20 withdrawal then the account should be 1080``() =
        let transactions = [date1,Deposited(100); date0,Deposited(1000)]
        Given transactions
        |> When date2 (Withdraw 20) 
        |> Then { Balance = 1080; 
                    Transactions = (date2, Withdrawed(20)) :: transactions }

    [<Fact>]
    let ``with transactions when query deposits then only deposits should be retrieved``() =
        [date2,Withdrawed(20);date1,Deposited(100); date0,Deposited(1000)]
        |> reduce Deposits
        |> ``should equal`` [date1,Deposited(100); date0,Deposited(1000)]

    [<Fact>]
    let ``with transactions when query withdrawals then only withdrawals should be retrieved``() =
        [date2,Withdrawed(20);date1,Deposited(100); date0,Deposited(1000)]
        |> reduce Withdraws
        |> ``should equal`` [date2,Withdrawed(20)]

    [<Fact>]
    let ``with transactions when Last 2 then only the 2 lasts deposits should be retrieved``() =
        [date2,Withdrawed(20);date1,Deposited(100); date0,Deposited(1000)]
        |> reduce (Last 2)  
        |> ``should equal`` [date2,Withdrawed(20);date1,Deposited(100)]

    [<Fact>]
    let ``with transactions when order by date and last 2 then only first 2 should be retrieved``() =
        [date2,Withdrawed(20);date1,Deposited(100); date0,Deposited(1000)]
        |> reduce OrderByDate
        |> reduce (Last 2)
        |> ``should equal`` [date0,Deposited(1000);date1,Deposited(100)]
