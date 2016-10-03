module skeleton5

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming.
	Skeleton for assignment 5.
	* To be used in a project with the environment iTasks.
	* The executable must be inside the iTask-SDK directory of the Clean distribution, 
	  or one of its subdirectories. You can either put your program there, or use the
	  set executable option of the Project Options.
	* You can also use the -sdk commandline flag to set the path.
	  Example: -sdk C:\Users\johndoe\Desktop\Clean2.4\iTasks-SDK

*/

import iTasks
import StdArray // for the size of a String

:: Idea	:== Maybe Note
:: Name	:== String
:: TitleOfIdea :== String
:: Number :== Int

//:: NamedIdea = {name :: Name, idea :: Idea}

:: TitledIdea = {title :: TitleOfIdea, idea :: Idea, name :: Name, number :: Number}

//derive class iTask NamedIdea
derive class iTask TitledIdea

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task // >>= for sequential composition of tasks

editIdea :: Name -> Task TitledIdea
editIdea name =  enterInformation (name +++ " add your title") []
	        >>= \title . (enterInformation (name +++ " add the idea of " +++ title) [] 
	        >>= \idea. return {title = title, idea = idea, name = name, number = 1} )


editIdeas :: Name -> Task [TitledIdea]
editIdeas name = editIdea name >>= (\x. return [x])

mainTask =   doIdentified editIdea
		 >>= viewInformation "The result" []

Start :: *World -> *World
Start world = startEngine mainTask world
