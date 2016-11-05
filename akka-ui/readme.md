# JavaFX UI with Akka

## Purpose
The purpose of this project to prove that Akka can help us to build JavaFX ui in a proper way. So this will be a POC.


## Overview

The project should give answer to these questions:
* There should be behaviors which are responsible for the business tasks of the components (service calls, validation, etc)
* There should be controllers which are responsible for the visible parts of the application (show a label, dialog, handle clicks, etc)
* Behaviors should be testable separately.
* Behaviors should be able to be tested in integration with other behaviors, or the whole application.
* The concurrency handling should be provided by the framework, and the real UI tasks should be explicit
* The application should be event driven

## Event flow
The diagram shows the event flow:

![alt text](https://github.com/chromygabor/the-playground/blob/master/akka-ui/doc/AkkaUI.png "Event flow")

