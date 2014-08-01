HTTPNinja
=========

HTTPNinja is a SmallTalk framework to write Rest Api clients with minimal effort. It is heavily inspired from [HTTParty] in Ruby. The goal is to write fully functional Rest Api clients without writing any redundant code. 

Examples
==========
Let's create a client to connect to GitHub. 

 - Create a GitHub that extends RESTClient
```smalltalk
RESTClient subclass: #GitHub
	instanceVariableNames: ''
	classVariableNames: ''
	category: 'HTTPNinja'
```

 - Define a method baseUri to provide Base Url for the api
```ruby
GitHub>>baseUri
	^'https://api.github.com/'.
```

 - (Optional) Specify the output format of the Api as XML or JSON. HTTPNinja will automatically convert XML or JSON output to Smalltalk data structures 
```ruby 
GitHub>>outputFormat
	^#json
```

 - GitHub uses Basic Authentication. Provides methods for username and password to use Basic authentication.
```ruby 
GitHub>>username
	^'manojkr'.
```
```ruby 
GitHub>>password
	^'secret'.
```

 - That's it. Now you are all set to start invoking different api methods. 
 - Let's write a method to get User Information on GitHub
```ruby 
GitHub>>user:username
	^self get: 'users/',username.
```
Another method to get repositories for a user.
```ruby 
GitHub>>userRepos:username
	^self get: '/users/',username,'/repos'.
```
 - Let's try using this GitHub client
```sh
client := GitHub new.
userinfo := y user:'manojkr'.
userrepos := y userRepos:'manojkr'.
```

Version
----

0.1

Tech
-----------

Dillinger uses a number of open source projects to work properly:

* [Zinc HTTP Components] - an open-source Smalltalk framework to deal with the HTTP networking protocol
* [Zinc SSO] - framework for implementations of client side OAuth & OpenID & SSL
* [NeoJson] - an elegant and efficient standalone Smalltalk framework to read and write JSON converting to or from Smalltalk objects

Installation
--------------
Coming soon

License
----

MIT

**Free Software, Hell Yeah!**

[HTTParty]: https://github.com/jnunemaker/httparty
[Zinc HTTP Components]: http://zn.stfx.eu/zn/index.html
[Zinc SSO]: https://github.com/svenvc/docs/blob/master/zinc/zinc-sso-paper.md
[NeoJson]: http://stfx.eu/neojson/
