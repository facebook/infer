package org.apache.commons.lang3;

public final class StringUtils {

  public static boolean isNotEmpty(final CharSequence cs) {
    return !isEmpty(cs);
  }

  public static boolean isEmpty(final CharSequence cs) {
    return cs == null || cs.length() == 0;
  }

  public static boolean isBlank(final CharSequence cs) {
    final int strLen = length(cs);
    if (strLen == 0) {
      return true;
    }
    for (int i = 0; i < strLen; i++) {
      if (!Character.isWhitespace(cs.charAt(i))) {
        return false;
      }
    }
    return true;
  }

  public static int length(final CharSequence cs) {
    return cs == null ? 0 : cs.length();
  }

}
