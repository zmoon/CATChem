# CATChem

## Description
The Configurable ATmospheric Chemisty (CATChem) component that includes all chemical and aerosol processes needed to perform
atmospheric chemistry and composition simulations within a model through a
flexible, easy to modify, and well-documented infrastructure. CATChem will
include the following processes: chemical kinetics, aerosols, photolysis,
wet deposition, dry deposition, connections to emissions, and connection to
physics schemes. The first use of CATChem will be connecting it to NOAA's
Unified Forecasting System (https://ufscommunity.org/) to create UFS-Chem. CATChem and UFS-Chem are
currently under development and more information will be provided soon.

## Warning
CATChem is currently under development. The code is public to
encourage collaboration amongst the community. Do not publish results using
CATChem without consulting the development team.


## Developer's Guide

### Description of Branches

#### main
This is the parent branch which
consolidates the current development in the repository.

#### release-vX.Y.Z
These represent stable release branches.
Users should always check out the most recent stable release branch
when cloning the repository.

### How to incorporate updates to CATChem

In order to contribute code to CATChem, you will need to fork the
repository, make changes on your fork, and submit a pull request with your
changes.
```
a. Fork the GitHub repository to your own GitHub account
    using the "Fork" button near the top right:

    [https://github.com/ufs-community/CATChem](https://github.com/ufs-community/CATChem)

    **_NOTE__**
       You can pull updates from the main repository
       by using the "Fetch Upstream" button on your fork.
       Alternatively: [#clone]_ ::

          $ git remote add upstream git@github.com:ufs-community/CATChem.git
          $ git pull upstream main
          $ git push origin main

(b) Navigate on your working machine
    to where you would like to keep the CATChem code
    (e.g. in your work location) and clone [#clone]_ your fork::

       $ git clone git@github.com:$GitHubUsername/$ForkName.git

(c) Checkout the develop branch --- you need to do this with the remote branch
    as well as create a local tracking branch::

       $ git checkout origin/develop
       $ git checkout develop

(d) Make changes to your fork.

(e) Submit a pull request back to the main CATChem repository with your
    changes.

(f) Select two code reviewers (see list under development team section).

(g) Once those two reviewers approve the code, it can be merged into the develop branch.

.. _clone-notes:
.. [#clone] Note that in order to do an SSH clone,
   e.g. ::

      $ git clone git@github.com:ufs-community/CATChem.git

   you must have already
   `added an SSH key <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account>`__
   to your GitHub account for your current machine.
   If you are new to GitHub, check out
   `this GitHub tutorial <https://jlord.us/git-it/>`__.
   We recommend the SSH method, but if you don't add an SSH key
   you can still clone the repositories via HTTPS, e.g. ::

       $ git clone https://github.com/ufs-community/CATChem.git

       ```

## Development Team

|  Organization |  Reviewers (github username) |
| ------------- | -------------------------------|
| NOAA ARL    | Zach Moon (`zmoon`)           |
|             | Barry Baker (`bbakernoaa`)    |
|             | Patrick Campbell (`drnimbusrain`)|
|             | Lacey Holland (`lholland-noaa`)|
| NOAA CSL    | Colin Harkins (`colin-harkins`)|
|             | Jian He (`jianheACM`) |
|             | Quazi Rasool (`quaz115`) |
|             | Becky Schwantes (`rschwant`)|  
| NOAA GSL    | Jordan Schnell (`jordanschnell`) |
|             | Kate Zhang (`zhanglikate`) |
| NOAA GML    | TBD |
| NOAA EMC    | TBD |
