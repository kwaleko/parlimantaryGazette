module Core.Test where

import Core.Types
import Core.Prosperity
import Core.Depression
import Core.Capping

defense = Category "Defense"

welfare = Category "Science"

science = Category "Science"

bill1 = Bill "Act to Construct the Great Wall of Malodivo"  defense 200000

bill2 = Bill "An Act to Construct Shelters for the Homeless" welfare 40000

bill3 = Bill "An Act to Fund the Development of Longer-Lasting Paper" science 14000

bill4 = Bill "An Act to Increase Retirement Benefits for Veterans" welfare 90000

bills =[bill1,bill2,bill3,bill4]

categoryFunding1 = CategoryDefaultFunding defense 1000

categoryFunding2 = CategoryDefaultFunding welfare 3000

categoryFunding3 = CategoryDefaultFunding science 5000

categoryFundings = [categoryFunding1,categoryFunding2,categoryFunding3]

specFund1 = BillSpecificFunding "An Act to Increase Retirement Benefits for Veterans" 500

specFund2 = BillSpecificFunding "An Act to Fund the Development of Longer-Lasting Paper" 7500

specFunds = [specFund1,specFund2]

cap1 = Cap defense 500

cap2 = Cap science 2000

pololene = District "pololene" 1000 categoryFundings specFunds [cap1,cap2]


categoryFunding1' = CategoryDefaultFunding defense 1000

categoryFunding2' = CategoryDefaultFunding welfare 3000

categoryFunding3' = CategoryDefaultFunding science 5000

categoryFundings' = [categoryFunding1,categoryFunding2,categoryFunding3]

specFund1' = BillSpecificFunding "An Act to Increase Retirement Benefits for Veterans" 500

specFund2' = BillSpecificFunding "An Act to Fund the Development of Longer-Lasting Paper" 10000

specFunds' = [specFund1',specFund2']

lakos = District "Lakos" 8000 categoryFundings' specFunds' []
