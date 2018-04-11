# parliamentaryGazette
## Modules
## Core (business logic)
### Interfaces.hs
in order to follow the hexagonal, clean or port and adapter architecture, I haven't specified IO monad for reading and writing to file, instead, typeclasses are use to provide with abstract function to be used for implemeting only the business rules and differing the mechanism of reading and writing output to later on. However, this approach is also useful for testing the effectful code in a pure manners


### Session.hs
After the meeting of the parlimentary session when the members of all districts have already decided their wishes on what to and the 
rules are applied, I have divided the problem to the below uses cases and created each use case in a module so any updated or modification for a specific use case could be done easily:

- Prosperity
- Depression
- Capping

### Capping.hs 
this module contains the Capping use case when there is a cap for each bill group and each district should abide by this law
and do not exceed the capping amount for the bill category

### Prosperity.hs
this module contains the use case when the total fund for a specific bill exceed the needed fund

### Depression.hs
this module contains the use case then the the total amount a district wishes to pay exceed the collected tax amount.

## Adapter
### JSON.hs
implementation of the abstract monad used in the business logic

## Effect 
this is the glue between the Core and Adapter, in the Effect, the Monad is a monad transformer of Reader over IO, IO to access file system and reader is to avoid the headache of passing the system configuration(filePath here) as parameter
