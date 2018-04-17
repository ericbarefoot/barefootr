##	converting CAMSIZER .xle file to csv and reading into Rdata form
##	Eric Barefoot
##	June 2017

#path = '/Users/ericfoot/Desktop/GSMA_2017/'
#
#outpath = '/Users/ericfoot/Desktop/GSMA_2017/data/'
#
#outfile = 'GSMA_2017_06_data.rda'

# require(tools)

# args = commandArgs(trailingOnly = T)
#
# path = args[1]
#
# if (length(args)==0) {
# 	stop("At least one argument must be supplied. Input file(s) path", call.=FALSE)
# } else if (length(args)==1) {
# 	# default output path
# 	args[2] = path
# 	args[3] = 'data.rda'
# } else if (length(args)==2) {
# 	args[3] = 'data.rda'
# }
#
# outpath = args[2]
#
# outfile = args[3]

camsizerxle = function(path, outpath, outfile) {

#	path is the path where all the data from the camsizer is stored.
#	outpath is the path where the csv files and rda file should be saved.

	if (substr(path, nchar(path), nchar(path)) != '/') {path = paste0(path, '/')}

	if (substr(outpath, nchar(outpath), nchar(outpath)) != '/') {outpath = paste0(outpath, '/')}

#	if outpath doesn't exist, this will create the directory:

	ifelse(!dir.exists(outpath), dir.create(outpath), FALSE)

	files = paste0(path,list.files(path))

	files = grep('.xle$', files, value = T)

	for (i in 1:length(files)) {
		system(paste0('ssconvert ', files[i], ' ', substr(files[i],1,nchar(files[i])-4), '.csv'), ignore.stderr = T)
	}

	files = paste0(path,list.files(path))

	csvfiles = grep('.csv$', files, value = T)

	data = list()

	for (i in 1:length(csvfiles)) {

		run_info = t(read.csv(csvfiles[i], nrows = 1, header = F)[,1:6])

		rownames(run_info) = c('raw_data_file','task_file','size_measurement','date','start_time','duration')

		summary = read.csv(csvfiles[i], skip = 9, nrows = 14, header = F)[,1:2]

		colnames(summary) = c('measurement', 'value')

		distribution = read.csv(csvfiles[i], skip = 24, nrows = -1, header = T)


		filename = file_path_sans_ext(basename(csvfiles[i]))

		data[[filename]] = list(run_info=run_info,summary=summary,distribution=distribution)

		system(paste0('mv ', csvfiles[i], ' ', outpath))

	}

	save(data, file = paste0(outpath, outfile))

}

camsizerxle(path,outpath,outfile)





#	do the column names stay the same all the time? what about the sections? this is something to investigate
#	if not, this piece of code can rename them:

#		colnames(distribution) = c('bin_min[micron]','bin_max[micron]','p3[%]','Q3[%]','1-Q3[%]','q3[%/micron]','SPHT3','Symm3','b/l3','PDN')
