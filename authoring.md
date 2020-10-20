# Authoring Stories

Ready to write your own [phlebotinum story](readme.md)?  All you need
is your own [github account](https://github.com/join).  Once you have
that, you don't need any special tools; github allows you to
[edit files directly](https://docs.github.com/en/github/managing-files-in-a-repository/managing-files-on-github)
online.  (If you'd prefer to use your own text editor, then you'll
want to learn how to set up a github sandbox on your own machine.)

After signing into your github account, you can either

* [create a new repository](https://docs.github.com/en/github/getting-started-with-github/create-a-repo)
    * [do this now on github](https://github.com/new)
* or
[fork an existing repository containing a story you want to modify](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo)
    * to do this for the example story, [visit the repository page](https://github.com/lingeringsocket/hello-phlebotinum) and then click the Fork button at the top right

Either way, be sure that your new repository is public, otherwise
phlebotinum won't be able to access it.

*Note that when you're first getting started, you can ignore all the
extraneous instructions about creating good commit messages, choosing
licenses, etc.  Those become important only when you're ready to share
your stories with others.*

Once your story is published in your repository, you can use the form below to try playing it by entering `github-username/repository-name`.  For example, if your github username is `bmoriartyb` and your repository name is `trinity`, you would enter `bmoriartyb/trinity`.

<form action="http://demo.phlebotinum.xyz:8000" target="_blank">
  <input type="text" id="arg" name="arg" size="50"/>
  <input type="submit" value="Play"/>
</form>
<br>

Phlebotinum will attempt to load your story definition from github,
parse all the axioms, and then start interpreting player commands.
If there's a problem, best of luck trying to make sense of the error messages!

After your story is successfully loaded for the first time, you might
want to bookmark the generated URL so that in the future you can skip the form entry above.

## Revisions and Caching

Phlebotinum always pulls the latest revision of your story from
github.  If that revision has previously been loaded successfully,
then phlebotinum may use an automatically cached save of the initial
state of the world, speeding startup considerably.  However, on the
first try, it will have to parse all of the files constituting your
story; this may take a while.

*There may be other cases in which the cached save isn't available, in
 particular after phlebotinum itself is upgraded.*
