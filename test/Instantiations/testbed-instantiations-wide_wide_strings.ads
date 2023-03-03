With EVIL.Util.Strings;
Package Testbed.Instantiations.Wide_Wide_Strings is
  new EVIL.Util.Strings(
     Character	=> Wide_Wide_Character,
     String	=> Wide_Wide_String,
     Space	=> ' '
    );
