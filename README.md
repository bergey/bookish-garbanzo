## bookish-garbanzo

I see structured logging as a step on the path from printf-debugging to distributed observability.  I like JSON for logs because logging tools support searching & filtering without caring what language the application is in, and the application doesn't need to care how the logs will eventually be processed.

This package has an effect class for logging, with:
- multiple log verbosity levels
- a Context, passed down the call stack & attached to each log message
- 

This is a bit more specialized than that, though.  It reports Haskell
`CallStack` info in a format understood by Google Stackdriver, so that
[Stackdriver Error
Reporting](https://cloud.google.com/error-reporting/) links to the
correct code.

### TODO

I may eventually get around to separating the three core ideas, which
someone might want to use independently:

- effect class, to log in programs not allowed arbitrary IO
- JSON as the representation
- Stackdriver-specific representations of call stack and service name

Another wishlist-goal is to engineer a `LogEncoding` class that can
default to `ToJSON` but supports separate instances.  I'd like this
for types which should never be sent to API clients, or which should
include more (or less?) information when logged than when sent to
application clients.
