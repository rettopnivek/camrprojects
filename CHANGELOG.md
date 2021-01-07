#### 0.1.2 (2021-01-07)

##### Documentation Changes

*  Updated documentation for extract_unique_value. ([e3f52436](https://github.com/rettopnivek/camrprojects/commit/e3f52436205e043e74f1409514acae8cfb3a9513))

#### 0.1.1 (2021-01-07)

##### Documentation Changes

*  Updated documentation for redcap_read to use markdown syntax. ([7d85b47a](https://github.com/rettopnivek/camrprojects/commit/7d85b47aae1aece90b77d754312b84756c952bf6))
*  changed man page title for build_demo_table to be title case ([eb3b94e9](https://github.com/rettopnivek/camrprojects/commit/eb3b94e94946625da99b0a515ac38f4a8978e121))

##### New Features

*  Added allow_multiple argument for extract_unique_value function. ([8fd676fe](https://github.com/rettopnivek/camrprojects/commit/8fd676fe7c75f56d9b60d2b4a37d57fc1eb0045c))
*  Added function to find lower and upper limits to pass into linear_interp function ([3a4e0be8](https://github.com/rettopnivek/camrprojects/commit/3a4e0be879ba1e7664ff08b72509ca2b500ebcfa))
*  Added redcap_read function to mimic REDCapR package more robustly. ([2ad9833e](https://github.com/rettopnivek/camrprojects/commit/2ad9833eaa0f6be4744e9b6a979f907e46440b4b))
*  Added functions for updating standardized file names and checking documentation for custom functions ([b5216d3e](https://github.com/rettopnivek/camrprojects/commit/b5216d3eafb01571d1b5bd0f3d6b2aca8fe7d86c))
*  added print out of script errors for run_processing_script function ([c0a47fcc](https://github.com/rettopnivek/camrprojects/commit/c0a47fcc3ed9f8bca61b4c5d3a3506352ca918b9))
*  added functionality to match_and_assign function to allow for conditional updating of vector based on user specified conditions ([3aac51b9](https://github.com/rettopnivek/camrprojects/commit/3aac51b9416d60dbb1abda701ce6acb4a2121ec9))
*  added function to match and assign new values based on input vector ([4e891d15](https://github.com/rettopnivek/camrprojects/commit/4e891d15a3c84f690575dd7070416cf43a17f115))
*  added function to add meta data for data dictionary purposes ([2f4f8087](https://github.com/rettopnivek/camrprojects/commit/2f4f808730f9c67f253175ae1e14d9bfebf3dcb3))
* **file_present:**  Added std_name parameter for checking whether old version of standardized file exists. ([66288c61](https://github.com/rettopnivek/camrprojects/commit/66288c61fe8584f39e99970bf28bc4988fb7080b))

##### Bug Fixes

*  Added option for interpolation to use first or last set of values as approximation for cases that fall outside range of provided vectors ([113f5ca6](https://github.com/rettopnivek/camrprojects/commit/113f5ca68accbbe173242d7547f88fa4f30e2f57))
*  run_processing_scripts now correctly checks whether user defined processing functions exists ([f4c893d2](https://github.com/rettopnivek/camrprojects/commit/f4c893d2fd55f470c30085d8105b4cf8dfea5fcb))
*  removed R history file ([90ec792c](https://github.com/rettopnivek/camrprojects/commit/90ec792c1bed4b788a92f2d8df99d6e38e71dfce))
*  Examples for create_standardized_filename now have all closing parentheses ([35e869cd](https://github.com/rettopnivek/camrprojects/commit/35e869cd7769f11b557ac9594d4d845248db95b4))
* **redcap_read:**  Fixed bug that caused raw_or_label argument to have no effect. ([0386a84a](https://github.com/rettopnivek/camrprojects/commit/0386a84a0f57c0b17e09c65632f5d7a25c3cd10f))
* **file_present:**  Returns actual match and not all standardized files in directory now. ([95b4649d](https://github.com/rettopnivek/camrprojects/commit/95b4649dffa7ded336aefae4f7fe985b7197a4d8))

##### Other Changes

* //github.com/rettopnivek/camrprojects ([9130d5fe](https://github.com/rettopnivek/camrprojects/commit/9130d5fe0fd3520d35434c9625c910d63bca88ff))

##### Refactors

* **redcap_read:**  Switched dependency from RCurl to httr to avoid Windows OS compatability issues. ([39193c67](https://github.com/rettopnivek/camrprojects/commit/39193c6780d02e36c55eb871ab60f41bb4f0f468))

### 0.1.0 (2020-09-22)

##### Build System / Dependencies

*  Updated gitignore to ignore .Rhistory files ([27615e90](https://github.com/rettopnivek/camrprojects/commit/27615e90085565c436a0e8f0b87ce778215ce1d6))
*  Remove .Rhistory from git ([6f15a2cc](https://github.com/rettopnivek/camrprojects/commit/6f15a2ccd222433590cf6762c01b27c9f4792d0c))
*  Updated DESCRIPTION and NAMESPACE to reflect new package imports ([033bc636](https://github.com/rettopnivek/camrprojects/commit/033bc63654cdf54bbb15b54ffd22242e1303e301))

##### Documentation Changes

*  Updated package dependencies in README ([109970b3](https://github.com/rettopnivek/camrprojects/commit/109970b3009f01a17fb32e03bd826a3499799497))

##### New Features

*  build_demo_table now automatically reports n's for each group ([346a879e](https://github.com/rettopnivek/camrprojects/commit/346a879edb7566fd6ff35f5c7c2fdfe22517c92c))
*  Added build_demo_table function for easy demographics table creation ([1061c73f](https://github.com/rettopnivek/camrprojects/commit/1061c73fb79842ff805cab789ef1c0e403db66d6))
*  added a function to save data files in a standardized format ([06752b23](https://github.com/rettopnivek/camrprojects/commit/06752b2357bf1650576524fcece623624a42757a))
*  added function handling portable component of running specified data processing scripts for the create_cleaned_data functions ([ba8ddc17](https://github.com/rettopnivek/camrprojects/commit/ba8ddc17fff61f231d9dfc3351c4eac0e4c3f562))

##### Bug Fixes

*  Updated name of build_demo_table script to follow numbering scheme ([c8802842](https://github.com/rettopnivek/camrprojects/commit/c8802842b2dc6bc9d9e61ddfad4504d1cad90f59))
*  fixed scoping issue with save call in save_data_files function ([72b96ef7](https://github.com/rettopnivek/camrprojects/commit/72b96ef7ce2fcf93bafff51b520f2e08e4ee303c))
*  fixed typo in save_data_files function for extensions ([7b235b75](https://github.com/rettopnivek/camrprojects/commit/7b235b758afd822cfc8452932fb91dafd0c766ca))
*  create_standardized_filename now checks to ensure description matches entire file description when auto-numbering. ([1e33ef90](https://github.com/rettopnivek/camrprojects/commit/1e33ef90473fe90aa5f9fe0ac229719578a6f3db))

##### Other Changes

* //github.com/rettopnivek/camrprojects ([14dce081](https://github.com/rettopnivek/camrprojects/commit/14dce081e877738522695017b1d82f00348e9dfc))

#### 0.0.2 (2020-09-19)

##### Bug Fixes

*  Updated package.json repository field so auto-changelog has working hyperlinks ([5c71bd43](https://github.com/rettopnivek/camrprojects/commit/5c71bd43824099e2c580b554e42b13d5f337b496))

#### 0.0.1 (2020-09-19)

##### Documentation Changes

*  removed history file ([0dc4d4b1](https://github.com/rettopnivek/camrprojects/commit/0dc4d4b1e7ef19a487967754f3ebce1e1de0f1ff))
*  updated manual pages ([097ead8c](https://github.com/rettopnivek/camrprojects/commit/097ead8c91ee3b5fcfbdeb8c53f4d4613b8c8958))
*  added documentation for version and file name creation functions ([93011dc8](https://github.com/rettopnivek/camrprojects/commit/93011dc85d889b2e5304098c22d8f76494116ecd))

##### New Features

*  Added package.json for auto changelog generation and version tagging ([8f36ff94](https://github.com/rettopnivek/camrprojects/commit/8f36ff94f5a022547a7e0d761bd45b61706410cf))
*  added function to extract unique values from data frame or list ([33a3f972](https://github.com/rettopnivek/camrprojects/commit/33a3f97206d1d632705c3c4c51b3366847ef6e0a))
*  added utility and statistic functions ([e8916bd1](https://github.com/rettopnivek/camrprojects/commit/e8916bd1d2563d270abb098a83999d54b337cfaa))
*  added functions for git version and filename creation ([168dd19e](https://github.com/rettopnivek/camrprojects/commit/168dd19e0e14d2e05b272c31d457573fb8793613))
*  initial commit for R package ([8a1d0cf4](https://github.com/rettopnivek/camrprojects/commit/8a1d0cf460c7c59e24456cb0336df576d9e090cf))

##### Bug Fixes

*  create_standardized_filename now recognizes different extensions when auto-increment file tag number ([d788e23c](https://github.com/rettopnivek/camrprojects/commit/d788e23cb5cd8644b9ae467e4cb0221526e66bda))
*  upload_to_folder function properly unlists path labels now ([4d7de1e2](https://github.com/rettopnivek/camrprojects/commit/4d7de1e2a3846619054df48b2f9fc7b49d80787f))
*  fixed typo in pull_git_version ([8e868376](https://github.com/rettopnivek/camrprojects/commit/8e8683763fc61f90f44234ff62dab6100dc8814a))
*  no longer overwrite API token ([790bf26a](https://github.com/rettopnivek/camrprojects/commit/790bf26adae9ebfef451ed7bdf45109d43c186e3))
*  fixed slot title for R script labels ([be6707c4](https://github.com/rettopnivek/camrprojects/commit/be6707c499ff376e9d6783996312340b8529eb41))
*  folder_pathways list properly updated ([9ef088de](https://github.com/rettopnivek/camrprojects/commit/9ef088de0519d434d0df3ee082dba5ecfc984dae))

##### Other Changes

* //github.com/rettopnivek/camrprojects ([e61a9972](https://github.com/rettopnivek/camrprojects/commit/e61a9972646bd8790c5329f8cbe0684b4577eac4))

