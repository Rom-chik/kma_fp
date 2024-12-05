## Results for each lab:
```
Work01 - 3/3 
Work02 - 3/3
Work03 - 5/6    1(-)
Work04A - 5/5   (resubmission)
Work05 - 2/6    1(4), 3b(2,3), 3c(2,3),4a(4), 4b(1-3),5b(1-3),6a(-),6b(-),7(-)
Work06 - 2/6    2a(1),2c(2),3b(2),4a(2),4b(1),5a(2),5b(2),6a(=), 6b(=),6c(=),7(=)
Work07 - 4/6    1(4),2(1,3,4,5,6), 7(4), 8(5)
Work08 - 4/6    1Int(4),1Char(1), 4(2-4), 6(8),9(-)
Work09 - 6/6
Work10 - 3/6    3b(3), 4b(2),5b(2-4), 6(1-6),8(2-6)
Work11 - 4/6    6(-), 7(2)
Work12 - 5/6    6(4),7(2)
Test00 - 26/29  3(5), 5(-), 12(3)
```

## Setting Up GHCup

For Windows, run this in a PowerShell session: 

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

## To run haskell files:

open cmd as administrator

move to directory with .hs file

write ```ghci FileName.hs```

call function in cmd ```multiply(3 5)  =  15```
