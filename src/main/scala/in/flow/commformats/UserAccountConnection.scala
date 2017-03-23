package in.flow.commformats

import in.flow.commformats.InternalCommFormats.UserConnectionType.UserConnectionType

/** direction forward means that the current user is the 'from' user */
case class UserAccountConnection(ctype: UserConnectionType, direction_forward: Boolean)
