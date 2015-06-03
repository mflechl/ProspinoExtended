job_count = 0

pdfsets = [("CT10.LHgrid", 53),
	   ("LHECNLO_EIG.LHgrid", 35),
           ("MSTW2008nlo68cl.LHgrid", 41),
           ("NNPDF21_100.LHgrid", 101),
           ("HERAPDF10_EIG.LHgrid", 21),
           ("abkm09_5_nlo.LHgrid", 26)]

from prospino_inp import *
from bsub_inp import *

for pdf in pdfsets:
	for i in range(0, 76):

		m_g = 500.0 + i * (7500.0/75)

		for mem in range(0, pdf[1]): 
			job_count += 1

			f = file("inputs_propaganda/input%d" % job_count, "w")

			desc = {
				"collider" : 1,
				"final_state" : "gg",
				"particle1" : 1,
				"particle2" : 1,
				"pdf_lo" : pdf[0],
				"member_lo" : mem,
				"use_pdfals_lo" : ".true.",
				"pdf_nlo" : pdf[0],
				"member_nlo" : mem,
				"use_pdfals_nlo" : ".true.",
				"inverse_scafac" : 1.0,
				"scavar" : 1.0,
				"cH_mass" : 300,
				"b_mass" : 4.75,
				"g_mass" : m_g 
			}

			f.write(prospino_input % desc)
#
#			f.write("%s\n" % pdf[0])
#			f.write("%d\n" % mem)
#	        	f.write(".true.\n")
#			f.write("%s\n" % pdf[0])
#			f.write("%d\n" % mem)
 #       		f.write(".true.\n")
#			f.write("1.0\n")
 #       		f.write("4.75\n")
#	        	f.write("172.5\n")
#			f.write("%d\n" % 300)
#			f.write("%d\n" % m_g) 
#
			f.close()

make_bsub_files("propaganda", "0:30", "2048", job_count)

print "Created inputs for %d jobs." % job_count	
