# WPF utilities for Clojure.

**PLEASE use the stable branch for now.** The current API should be considered
usable but unstable.

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

## Usage

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
```target-path``` in ```doat``` and ```async-doat``` forms.

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
also bind the value pointed to by target-path to the *cur* var.

**Example:**
```clojure
(doat myElement (.SomeMethod *cur* arg1 arg2))
```

#### ````caml```` macro
```(caml xaml-ns-map? element-type-keyword [& property-event-setter-pairs] & children)```

The caml macro is the ClojureWpf analogue to XAML. It actually uses some of the
XAML infrastructure under the hood to do its job.

#### ```async-at``` macro
The asynchronous variant of ```at```. Uses Dispatcher.BeginInvoke instead of
Dispatcher.Invoke under the hood. The syntax is identical to ```at``` otherwise.

#### ```async-doat``` macro
The asynchronous variant of ```doat```. Uses Dispatcher.BeginInvoke instead of
Dispatcher.Invoke under the hood. The syntax is identical to ```doat```
otherwise.

### ```property-event-setter-pairs``` syntax for ```at```, ```async-at```, and ```caml```
TODO

### ```target-path``` syntax for ```at```, ```doat```, ```async-at```, and ```async-doat```
TODO

### Helper Functions and Macros

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

## License

Distributed under the Eclipse Public License, the same as Clojure.
