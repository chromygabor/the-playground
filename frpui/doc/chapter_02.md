Basic blocks of the framework
====

Following the last chapter, know we have a basic vision, how a framework should look like.
The requirements which are built on the top of the basics are:
- We will need a model which can step itself from one state to another by an action
- These states should be able to be identified uniquely
- We will need somehow perform side effects (rendering the UI), by previously computed states.
- The state update should perform not on the same thread as the render does
- The side effect should contains computation as less as it can
- The side effect should be completly separated from the state update.
