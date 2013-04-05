# WPF utilities for Clojure.

**PLEASE use the stable branch for now.** The current API should be considered
usable but unstable.  It is possible that there will be slight changes to the
syntax described below until this library reaches 1.0.

Here is a short gist that will give you some idea of how the library can be
used: https://gist.github.com/aaronc/fab70684cb52803a983f


## Simple Example

```clojure
user=> (use 'ClojureWpf.core)
nil
; Creates a button with the text "Hello World"
user=> (def myButton (caml :Button [:Content "Hello World"]))
#'user/myButton
user=> (defn click-fn [sender event-args] (println "You clicked
me."))
#'user/click-fn
; Binds the Click event of myButton to the click-fn handler
user=> (at myButton :Click #'click-fn)
```

## Creating and Modifying WPF elements

The design of ClojureWpf was influenced by some elements of both Hiccup and
Enlive, so you may notice some similarities. It's syntax and features, however,
are uniquely targetted to the context of a .NET WPF environment.

The basic macros, functions, and vars that you will want to know to use the
framework effectively are documented below. ```at```, ```async-at``` and
```caml``` share a common syntax for ```property-event-setter-pairs```.
```at```, ```doat```, ```async-at```, and ```async-doat``` share common syntax
for ```target-path```.

#### ```*cur*``` dynamic var
The ```*cur*``` var is used to access elements
pointed to by ```target-path```'s (and also the current ```caml``` element) in
the context of ```property-event-setter-pairs``` in ```at```, ```async-at```,
and ```caml``` forms. It is also bound to the element pointed to by
```target-path``` in ```doat``` and ```async-doat``` forms, used by the
```deref``` function of attached properties, and bound in event handlers bound
using ```property-event-setter-pairs```.

#### ```at``` macro
```(at target-path & property-event-setter-pairs & nested-at-forms)```

The ```at``` macro resolves an element pointed to by ```target-path``` and
applies the ```property-event-setter-pairs``` to it in the context of its
dispatcher thread (this means that Dispatcher.Invoke called on the body of
macro). Optionally, ```nested-at-forms``` can be supplied _after_
```property-event-setter-pairs```. The syntax of a nested at form are simply the
arguments to at wrapped in a list. The starting element of ```target-path``` for
nested at forms will be the element of the parent form. at forms can be nested
multiple times. This allows use to modify whole trees of elements with a single
form. The ```*cur*``` var is bound to the element pointed to by
```target-path``` in the context of its ```property-event-setter-pairs```.

**Example:**
```clojure
(at myGrid
    :Width 500.0
    ([:myButton]
     :Click #'my-handler)
    ([:myTextBox]
     :Text "Some text"))
```

#### ```doat``` macro ```(doat target-path & body)```

The doat macro executes the body forms on the dispatcher thread of the element
resolved by target-path (this means that Dispatcher.Invoke has been called). It
also bind the value pointed to by target-path to the ```*cur*``` var.

**Example:**
```clojure
(doat myElement (.SomeMethod *cur* arg1 arg2))
```

#### ````caml```` macro
```(caml xaml-ns-map? element-type-keyword [& property-event-setter-pairs]? & children)```

The caml macro is the ClojureWpf analogue to XAML. It actually uses some of the
XAML infrastructure under the hood to do its job.

```xaml-ns-map?``` is an optional map of namespace prefixes mapped to
XamlSchemaContext's. Please use the ```xaml-ns``` function to construct
XamlSchemaContext's. Ex: ```{:myViews (xaml-ns "MyViewsNamespace"
"MyViewsAssembly")}```.  This is basically the equivalent of xmlns prefixes in
XAML (see [MSDN](http://msdn.microsoft.com/en-us/library/ms747086.aspx).)  This
allows us to specify WPF type names using this syntax: ```:myViews:MyView```.

```element-type-keyword``` is the the name of the WPF element you want to
construct. By default this will be resolved in the same default WPF namespaces
that XAML uses.  i.e. ```(caml :TextBox)``` or ```(caml :Grid)```.  If you
provide an optional first argument of ```xaml-ns-map?``` you can access types
defined in other namespaces and assemblies. An optional hash argument to the end
of the type name can be used to set the name property on the element (ex:
```:TextBox#myTextBox```) - this is basically a shortcut for the XAML
```x:Name``` attribute.

See the docs for ```property-event-setter-pairs``` for the syntax for this
optional ```caml``` element.  Note that these should be placed within a vector.

```children``` can be a single child element for types taking a single child, or
a list of elements for types that take a collection of children.  As in
```property-event-setter-pairs``` expressions, forms that look like
```(:MyElementTypeName ...)``` will be interpreted as ```caml``` forms if the
keyword resolves to a type in the provided or implicit XAML namespace context.

**Examples:**
```clojure
(caml :Grid
  (:TextBox#myTextBox [:Text "Hello World"])
  (:Button#myButton [:Click #'on-myButton-click] "Click Me"))

(caml {:myNs (xaml-ns "MyNamespace" "MyAssembly")}
 :myNs:MyUserControl)
```


#### ```async-at``` macro
The asynchronous variant of ```at```. Uses Dispatcher.BeginInvoke instead of
Dispatcher.Invoke under the hood. The syntax is identical to ```at``` otherwise.

#### ```async-doat``` macro
The asynchronous variant of ```doat```. Uses Dispatcher.BeginInvoke instead of
Dispatcher.Invoke under the hood. The syntax is identical to ```doat```
otherwise.

### ```property-event-setter-pairs``` syntax for ```at```, ```async-at```, and ```caml```

A property/event setter pair consists of a property/event setter key and a value
expression. property/event setter keys are usually Clojure keywords
corresponding to the name of CLR property or event member.  You can also provide
a keyword that corresponds to a WPF DependencyProperty defined on another
DependencyObject (i.e. an attached property) with the syntax ```:OtherType.AttachedProperty```.

#### Handling of Property value expressions
* If you pass a list beginning with a keyword that can be resolved to a XAML
  type in the default xaml context, the list will be converted to a ```caml```
  form.
* If the value expression is of type BindingBase and can be bound to the target
  object, a data binding on the specified property will be set.
* If the property is writeable
    - If you pass a value which is a Clojure function (i.e. satisifies ```fn?```), the
      current value of property will be passed to the function, and the property
      will be set to the return value of the function
    - Otherwise, the property will simply be set to the value that was passed to
      it (after evaluation)
* If the property is read-only
    - If you pass a Clojure function (satisfies ```fn?```), the value of the property
      will be passed to the function (for modification say in the case of a
      collection).
    - If you pass a Clojure seq (satisfies ```seq?```) and the property type satisfies
      System.Collections.ICollection, the .Clear method will be called on the
      current property value, and the elements of the seq will be added to the
      collection using its .Add method.
      
***Examples:***
```clojure
(caml :TextBox [:Text (:Binding "MyTextProperty")])
(at :ListBox [:Children ["abc" "def"]])
```

#### Handling of Event value expressions
You can provide either a Clojure IFn or a CLR Delegate instance as a value for
an event key.  If you pass a delegate, then it will simply be added as a handler
for this event.  If you pass a Clojure IFn, it will be wrapped in the
appropriate Delegate type for this event and the ```*cur*``` var will be bound
to object that you are setting the event handler on (this way attached
properties can be used from event handler fn's).

### ```target-path``` syntax for ```at```, ```doat```, ```async-at```, and ```async-doat```

```target-path```'s make use of the WPF LogicalTreeHelper/FindLogicalNode method
and are resolved as follows:
* If ```target-path``` is a vector, the first argument will be taken as the root element in
  a path expression if it is not a keyword.  If it is a keyword, the root of the
  path expression will be whatever ```*cur*``` is bound to - if it is not bound
  an error will be thrown.  Each successive keyword in the vector will be taken
  as an argument to repeated applications to LogicalTreeHelper/FindLogicalNode.
  So if we pass ```[myElement :myGrid :myTextBox]```, the resulting target will
  be:

```clojure
(LogicalTreeHelper/FindLogicalNode
  (LogicalTreeHelper/FindLogicalNode myElement "myGrid")
  "myTextBox")
```
* If ```target-path``` is anything else, this will the target.

Basically ```target-path``` is a convenient way of finding named child elements
in a XAML tree.

### Attaching user data to a WPF view

#### ```defattached```
```(defattached MyAttachedPropertyName)```

Defines an attachable WPF DependencyProperty that can be bound to any element in
the XAML tree and store user data.  One way to think of this as an atom that is
valid within the scope of the some XAML subtree.  Use the ```attach``` function
to set the attach property on a DependencyObject and the ```deref``` function or
```@``` reader macro to retrieve the value of the property while within an
```at``` or ```doat``` form.  Since DependencyProperty's have this nice feature
called [Property Value Inheritance](http://msdn.microsoft.com/en-us/library/ms753197.aspx) 
we can use it to retrieve the value of a DependencyProperty set on a parent
object in the tree from a child object.

**Example:**
```clojure
user=> (use 'ClojureWpf.core)
nil
user=> (defattached MyProperty)
#'user/MyProperty
user=> (def grid (caml :Grid (:TextBox#myTextBox)))
#'user/grid
user=> (attach MyProperty grid 5)
nil
user=> (doat [grid :myTextBox] @MyProperty)
5
```

### Using ClojureWPF for live WPF developement

#### ```separate-threaded-window```
Creates a WPF window a separate dispatcher thread that allows you to play with
WPF code from the repl.

#### ```load-dev-xaml```
Loads a XAML file that you have stored on disk (and which may later be compiled
into some C# assembly) and constructs the Xaml object it points to, removing any
troublesome references to C# code-behind (i.e. the x:Class attribute is stripped
from the root element). This allows you to load and reload XAML code from the
disk while in development mode (possibly using some tool like Expression Blend
to edit it) and then later compile it into a C# assembly.

### FAQ

_Why does the ClojureWpf namespace use CamelCase?  Isn't that against Clojure naming conventions?_

Yes, it is. But, originally the only way we had of packaging the library into a
DLL was to store as an embedded resource in a C# DLL. Also, we did and (still
do) want to provide some C# helper clases that can be used for WPF data binding
bundled into that DLL. ClojureWpf just seems like a more natural name for a C#
namespace and .NET DLL than clj_wpf. We could change it, but it's probably not
that important.

_Why isn't the caml syntax just like Hiccup?_

ClojureWpf uses the syntax ```(caml :TextBox [:Text "Hello"])``` as opposed to
the Hiccup-style ```(caml [:TextBox {:Text "Hello"}])```. The reason for this is
because there are some rare cases where either a) you need to use a
property/event key twice in the same expression or b) you need to know in which
order you're calling your property setters. You just can't do this with a
Clojure map, but you can with a vector. Why would anyone ever need this you
might ask. Well, I recall some early use case where I wanted to bind two
handlers to a single event (something like
```(caml :Button [:Click #'on-click :Click #'do-beep))```). Anyway, maybe it's
not so common, but we decided to support it. Also I like the fact that actions
specified by a vector happen in the order you write it rather than out of order 
if you use a map. So there you go. It follows since we're using the vector for
property/event setters, that lists need to be used for constructing elements.

## License

Distributed under the Eclipse Public License, the same as Clojure.
