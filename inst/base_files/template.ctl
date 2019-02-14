$models = <<'END_MODELS';
---
- name: BASEMODEL
  description: Base model
  referencemodel: BASEMODEL
  baseflags: ''
  flags: ''

- name: FINALMODEL
  description: Final model
  referencemodel: BASEMODEL
  vpc: true
  bootstrap: true
  baseflags: ''
  flags: ''

END_MODELS

$template = <<'END_TEMPLATE';
$PROB PLACEHOLDER

$INPUT
C RN ID TIME TAD DVID EVID AMT RATE BLQ DV AGEBL SEX RACE WTBL HTBL

$DATA PLACEHOLDER.csv
IGNORE=@
IGNORE=(TIME < 0)

$SUB ADVAN13 TOL=9

$MODEL

COMP=(CENT,DEFOBS,DEFDOSE)
COMP=(PERIPH)

$PK

MU_[nCL] = THETA[CL]
CL = EXP(MU_[nCL] + ETA[nCL])
ETA_CL = ETA[nCL]

MU_[nVC] = THETA[VC]
VC = EXP(MU_[nVC] + ETA[nVC])
ETA_VC = ETA[nVC]

[% IF ETA_on_VP %]
MU_[nVP] = THETA[VP]
VP = EXP(MU_[nVP] + ETA[nVP])
ETA_VP = ETA[nVP]
[% ELSE %]
VP = EXP(THETA[VP])
[% END %]

[% IF ETA_on_Q %]
MU_[nQ] = THETA[Q]
Q  = EXP(MU_[nQ] + ETA[nQ])
ETA_Q = ETA[nQ]
[% ELSE %]
Q  = EXP(THETA[Q])
[% END %]

[% IF ALLOMETRIC %]
; Allometric scaling
WTCL = THETA[WT_on_CL]*LOG(WTBL/70)
WTQ  = THETA[WT_on_CL]*LOG(WTBL/70)
WTVC = THETA[WT_on_V]*LOG(WTBL/70)
WTVP = THETA[WT_on_V]*LOG(WTBL/70)

CL = CL*EXP(WTCL)
Q  =  Q*EXP(WTQ)
VC = VC*EXP(WTVC)
VP = VC*EXP(WTVP)
[% END %]


K = CL/VC
K12 = Q/VC
K21 = Q/VP

S1 = VC
S2 = VP

$DES

DADT(1) = -K*A(1) - K12*A(1) + K21*A(2)
DADT(2) = K12*A(1) - K21*A(2)

$ERROR

CONC1 = A(1)/VC

ERRPROP = CONC1 * THETA[ERRPROP]
[% IF ERRADD %]
ERRADD = THETA[ERRADD]
ERR1 = SQRT(ERRPROP**2 + ERRADD**2)
[% ELSE %]
ERR1 = ERRPROP
[% END %]

IPRED = CONC1
Y = IPRED + ERR1*EPS[DUMMY]


INIT_THETA[ (-1, 1, 10) ; CL ]
INIT_THETA[ (-1, 1, 10) ; VC ]
INIT_THETA[ (-1, 1, 10) ; VP ]
INIT_THETA[ (-1, 1, 10) ; Q  ]
[% IF ALLOMETRIC %]
[% IF FIXALLO %]
INIT_THETA[ 0.75 FIX ; WT_on_CL  ]
INIT_THETA[ 1.00 FIX ; WT_on_V  ]
[% ELSE %]
INIT_THETA[ 0.75 ; WT_on_CL  ]
INIT_THETA[ 1.00 ; WT_on_V  ]
[% END %]
[% END %]
INIT_THETA[ (0, 0.1) ; ERRPROP ]
[% IF ERRADD %]
INIT_THETA[ (0, 10) ; ERRADD ]
[% END %]

INIT_OMEGA_BLOCK[2]
INIT_OMEGA[0.1       ; nCL]
INIT_OMEGA[0.05 0.1  ; nVC]
[% IF ETA_on_VP %]
INIT_OMEGA[0.1       ; nVP]
[% END %]
[% IF ETA_on_Q %]
INIT_OMEGA[0.1       ; nQ]
[% END %]

INIT_SIGMA[ 1 FIX ; DUMMY ]

[% IF VPC %]
$MSFI [% name %].msf
$SIMULATION (5076085) ONLYSIM TRUE=FINAL NSUBPROBLEMS=2000
$TABLE NOPRINT ONEHEADERALL NOTITLE NOAPPEND FORMAT=,1PE15.8 FILE=vpctable1.csv
   RN ID TIME MDV DV PRED
[% ELSE %]

$EST METH=COND INTER PRINT=5 SIGL=9 NSIG=3 NOABORT MAX=99999 MSF=[% name %].msf
[% IF NOT BOOTSTRAP %]
$COV PRINT=E

[% IF NOT NOTABLE %]
$TABLE NOPRINT ONEHEADERPERFILE NOTITLE NOAPPEND FORMAT=,1PE15.8 FILE=nmtable1.csv
    RN ID TIME TAD AMT EVID MDV DV PRED IPRED RES WRES CWRES NPDE
    CL VC VP Q ETAS(1:LAST)
[% END %]
[% END %]

[% END %]

END_TEMPLATE

$meta = <<'END_META';
---
data:
  full:     "placeholder"
  fitted:   "placeholder"
  nmtable:  "nmtable1.csv"
  vpctable: "vpctable1.csv"

parameters:
  - name: CL
    label: Clearance CL
    units: L/h
    trans: exp
    type: Structural

  - name: Q
    label: Intercompartmental clearance Q
    units: L/h
    trans: exp
    type: Structural

  - name: VC
    label: Central volume Vc
    units: L
    trans: exp
    type: Structural

  - name: VP
    label: Peripheral volume Vp
    units: L
    trans: exp
    type: Structural

  - name: WT_on_CL
    label: Weight on CL/Q
    expression: "&times; (WT/70)<sup><i>&theta;</i></sup>"
    type: CovariateEffect
    relatedTo: CL,Q

  - name: WT_on_V
    label: Weight on Vc/Vp
    expression: "&times; (WT/70)<sup><i>&theta;</i></sup>"
    type: CovariateEffect
    relatedTo: VC,VP

  - name: nCL
    label: On CL
    type: IIV
    relatedTo: CL

  - name: nVC
    label: On Vc
    type: IIV
    relatedTo: VC

  - name: nVP
    label: On VP
    type: IIV
    relatedTo: VP

  - name: nQ
    label: On Q
    type: IIV
    relatedTo: Q

  - name: om(nCL,nVC)
    label: Correlation CL, Vc
    type: IIV
    relatedTo: CL,VC

  - name: om(nCL,nVP)
    label: Correlation CL, Vp
    type: IIV
    relatedTo: CL,VP

  - name: om(nVC,nVP)
    label: Correlation Vc, Vp
    type: IIV
    relatedTo: Vc,VP

  - name: ERRPROP
    label: Proportional Error
    units: '%'
    trans: '%'
    type: RUV

  - name: ERRADD
    label: Additive Error
    units: '&mu;g/mL'
    type: RUV

labels:
  CONC:  "Drug Concentration (\u00B5g/mL)"
  DV:    "Observed Concentration (\u00B5g/mL)"
  PRED:  "Population Predicted Concentration (\u00B5g/mL)"
  IPRED: "Individual Predicted Concentration (\u00B5g/mL)"
  CWRES: "Conditional Weighted Residual"
  TAFD:  "Time After First Dose (hours)"
  TAD:   "Time After Dose (hours)"
  TAFDD: "Time After First Dose (days)"
  TADD:  "Time After Dose (days)"
  TAFDW: "Time After First Dose (weeks)"
  TADW:  "Time After Dose (weeks)"

namemap: [% NAMEMAP %]

df: [% DF %]

END_META

