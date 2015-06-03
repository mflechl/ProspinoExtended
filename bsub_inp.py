bsub_input = """#!/usr/bin/env zsh

#BSUB -J "myArray[%(start)d-%(end)d]" ARRAYJOB

module switch intel gcc/4.6
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/rk066459/LHAPDF/lib
%(extra)s
./prospino_2.run < %(directory)s/input$LSB_JOBINDEX
""" 

from subprocess import call

def make_bsub_files(name, time, memory, job_end, job_start=1, extra=""):
	job_num = job_start

	count = 1

	exec_file = file("exec_%s.sh" % (name), "w")
	exec_file.write("#!/usr/bin/env zsh\n")

	def write_exec_line(filename):
		exec_file.write("bsub -W %s -M %s -o job_%s%%I < %s\n" % (time, memory, name, filename))

	while job_num < job_end:
		filename = "%s_jobs%d.sh" % (name, count)
		bsub = file(filename, "w")

		desc = {
			"start" : job_num,
			"extra" : extra,
			"directory" : "inputs_%s" % name
		}

		temp = job_num

		job_num = min(job_num + 99, job_end)

		desc["end"] = job_num

		job_num += 1

		bsub.write(bsub_input % desc)		

		bsub.close()

		write_exec_line(filename)

		print "Create bsub-file for inputs %d to %d." % (temp, job_num-1) 

		count += 1

	exec_file.close()

	call("chmod 755 %s" % ("exec_%s.sh" % (name)), shell=True)
