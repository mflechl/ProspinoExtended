job_count = 0

pdfsets = [("CT10.LHgrid", 0, "cteq6ll.LHpdf", 0, True),
         ("MSTW2008nlo_asmzrange.LHgrid", 9, "MSTW2008lo68cl.LHgrid", 0, True),
         ("NNPDF23_nlo_as_0118.LHgrid", 0, "NNPDF21_lo_as_0130_100.LHgrid", 0, True)]
#           ("NNPDF21_as_0118_100.LHgrid", 101,"NNPDF21_lo_as_0130_100.LHgrid", 101, True)]


#MSTW2008nlo_asmzrange.LHgrid 9
#NNPDF23_nlo_as_0118.LHgrid 101
#CT10.LHgrid 0


from mh_detail import *
from prospino_inp import *
from bsub_inp import *

for m_H in m_Hs["8TeV"]:
	for pdf in pdfsets:
                for i in range(1,61): #1-60
			if (i>20 and (i%5)!=0): #do 1-20, then up to 60 in steps of 5
				continue

			job_count += 1

			f = file("inputs_tanbeta/input%d" % job_count, "w")

			desc = {
				"collider" : 3,
				"final_state" : "ht",
				"particle1" : 1,
				"particle2" : 1,
				"pdf_lo" : pdf[0],
				"member_lo" : pdf[1],
				"use_pdfals_lo" : ".true." if pdf[4] else ".false.",
				"pdf_nlo" : pdf[0],
				"member_nlo" : pdf[1],
				"use_pdfals_nlo" : ".true." if pdf[4] else ".false.",
				"inverse_scafac" : m_H[1],
				"scavar": 1.0,
				"scavar3": 1.0,
				"cH_mass" : m_H[0],
				"b_mass" : 4.75, 
				"tanbeta" : i
				}

			f.write(prospino_input % desc)
			f.close()

make_bsub_files("tanbeta", "0:20", "2048", job_count)

print "Created %d input-files." % job_count
