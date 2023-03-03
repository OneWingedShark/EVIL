With EVIL.Util.Strings;
Package Testbed.Instantiations.Wide_Strings is
  new EVIL.Util.Strings(
     Character	=> Wide_Character,
     String	=> Wide_String,
     Space	=> ' '
    );
