;; Get the beam name as XXYY
FUNCTION BAG_TSSUB2_BEAM_NAME,name

  IF STRLOWCASE(name) EQ '30lt' THEN RETURN,'30lt'
  IF STRLOWCASE(name) EQ '30l' THEN RETURN,'30lt'

  IF STRLOWCASE(name) EQ '30rt' THEN RETURN,'30rt'
  IF STRLOWCASE(name) EQ '30r' THEN RETURN,'30rt'

  IF STRLOWCASE(name) EQ '150lt' THEN RETURN,'15lt'
  IF STRLOWCASE(name) EQ '150l' THEN RETURN,'15lt'
  IF STRLOWCASE(name) EQ '15lt' THEN RETURN,'15lt'
  IF STRLOWCASE(name) EQ '15l' THEN RETURN,'15lt'

  IF STRLOWCASE(name) EQ '150rt' THEN RETURN,'15rt'
  IF STRLOWCASE(name) EQ '150r' THEN RETURN,'15rt'
  IF STRLOWCASE(name) EQ '15rt' THEN RETURN,'15rt'
  IF STRLOWCASE(name) EQ '15r' THEN RETURN,'15rt'

  IF STRLOWCASE(name) EQ '210lt' THEN RETURN,'21lt'
  IF STRLOWCASE(name) EQ '210l' THEN RETURN,'21lt'
  IF STRLOWCASE(name) EQ '21lt' THEN RETURN,'21lt'
  IF STRLOWCASE(name) EQ '21l' THEN RETURN,'21lt'

  IF STRLOWCASE(name) EQ '210rt' THEN RETURN,'21rt'
  IF STRLOWCASE(name) EQ '210r' THEN RETURN,'21rt'
  IF STRLOWCASE(name) EQ '21rt' THEN RETURN,'21rt'
  IF STRLOWCASE(name) EQ '21r' THEN RETURN,'21rt'

  IF STRLOWCASE(name) EQ '330lt' THEN RETURN,'33lt'
  IF STRLOWCASE(name) EQ '330l' THEN RETURN,'33lt'
  IF STRLOWCASE(name) EQ '33lt' THEN RETURN,'33lt'
  IF STRLOWCASE(name) EQ '33l' THEN RETURN,'33lt'

  IF STRLOWCASE(name) EQ '330rt' THEN RETURN,'33rt'
  IF STRLOWCASE(name) EQ '330r' THEN RETURN,'33rt'
  IF STRLOWCASE(name) EQ '33rt' THEN RETURN,'33rt'
  IF STRLOWCASE(name) EQ '33r' THEN RETURN,'33rt'

END
