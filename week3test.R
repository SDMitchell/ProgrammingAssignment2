require(RUnit)

rootdir = "~/Online Courses/Coursera/R Programming/Week 3/ProgrammingAssignment2"
tempFile <- file.path(rootdir, "tmp_test_cacheSolve.txt")

## Unit test for cachematrix method
test_cachematrix <- function()
{
	source(file.path(rootdir, "cachematrix.R"))

	# This one should work
	values <- c(2, 2, 2, 3, 4, 5, 6, 5, 5)
	data <- matrix(values, 3,3)
	wrappedData <- makeCacheMatrix(data)
	resultFirstTime <- cacheSolve(wrappedData)
	checkTrue(!is.null(resultFirstTime), msg="I'm guessing that the supplied matrix was not invertible")
	#print(resultFirstTime)

	# Should work the second time as well. We'll sink the output and look for the message
	# saying that we are accessing cached data
	tempFileConn <- file(tempFile, "w")
	sink(file=tempFileConn, type="message")
	resultSecondTime <- cacheSolve(wrappedData)
	sink(type="message")
	cachedText <- readLines(tempFileConn <- file(tempFile, "r"))
	flush(tempFileConn)
	close(tempFileConn)
	unlink(tempFile)
	checkTrue(!is.null(resultSecondTime), msg="I'm guessing that the supplied matrix was not invertible")
	checkEquals("getting cached data", cachedText)
	#print(resultSecondTime)

	# This one should fail as non-invertible
	values <- c(2, 2, 2, 3, 4, 5, 6, 5, 4)
	data <- matrix(values, 3,3)
	wrappedData <- makeCacheMatrix(data)
	checkException(cacheSolve(wrappedData), msg="I'm guessing that the supplied matrix was invertible; we weren't expecting it to be")
}

## Run this in the console to run the tests and print out the results
runSuite <- function()
{
	testSuite <- defineTestSuite("week3", dirs=rootdir, testFileRegexp=".*test\\.R", testFuncRegexp="^test_.+")
	testResult <- runTestSuite(testSuite)
	printTextProtocol(testResult, showDetails=TRUE)
}
