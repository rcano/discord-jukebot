package jukebox
package discord

import enumeratum.values._
import java.time.Instant

import Json4sUtils._
import Json.formats

object GatewayEvents {

  sealed abstract class EventType(val value: String) extends StringEnumEntry
  object EventType extends StringEnum[EventType] {
    val values = findValues
    case object Ready extends EventType("READY")
    case object Resumed extends EventType("RESUMED")
    case object ChannelCreate extends EventType("CHANNEL_CREATE")
    case object ChannelUpdate extends EventType("CHANNEL_UPDATE")
    case object ChannelDelete extends EventType("CHANNEL_DELETE")
    case object GuildCreate extends EventType("GUILD_CREATE")
    case object GuildUpdate extends EventType("GUILD_UPDATE")
    case object GuildDelete extends EventType("GUILD_DELETE")
    case object GuildBanAdd extends EventType("GUILD_BAN_ADD")
    case object GuildBanRemove extends EventType("GUILD_BAN_REMOVE")
    case object GuildEmojisUpdate extends EventType("GUILD_EMOJIS_UPDATE")
    case object GuildIntegrationUpdate extends EventType("GUILD_INTEGRATION_UPDATE")
    case object GuildMemberAdd extends EventType("GUILD_MEMBER_ADD")
    case object GuildMemberRemove extends EventType("GUILD_MEMBER_REMOVE")
    case object GuildMemberUpdate extends EventType("GUILD_MEMBER_UPDATE")
    case object GuildMemberChunk extends EventType("GUILD_MEMBER_CHUNK")
    case object GuildRoleCreate extends EventType("GUILD_ROLE_CREATE")
    case object GuildRoleUpdate extends EventType("GUILD_ROLE_UPDATE")
    case object GuildRoleDelete extends EventType("GUILD_ROLE_DELETE")
    case object MessageCreate extends EventType("MESSAGE_CREATE")
    case object MessageUpdate extends EventType("MESSAGE_UPDATE")
    case object MessageDelete extends EventType("MESSAGE_DELETE")
    case object MessageDeleteBulk extends EventType("MESSAGE_DELETE_BULK")
    case object PresenceUpdate extends EventType("PRESENCE_UPDATE")
    case object TypingStart extends EventType("TYPING_START")
    case object UserUpdate extends EventType("USER_UPDATE")
    case object VoiceStateUpdate extends EventType("VOICE_STATE_UPDATE")
    case object VoiceServerUpdate extends EventType("VOICE_SERVER_UPDATE")
    case object Unknown extends EventType("Unknown")
  }

  case class GatewayEvent(tpe: EventType, payload: () => DynJValueSelector)
  case class Ready(
    v: Int,
    user: User,
    privateChannels: Seq[DmChannel],
    guilds: Seq[UnavailableGuild],
    /* presences: Seq[Any],
     relationships: Seq[Any], */
    _trace: Seq[String]
  )
  object Ready { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.Ready) Some(Json.extract[Ready](ge.payload().d.jv)) else None }

  case object Resumed { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.Resumed) Some(()) else None }

  case class ChannelCreate(channel: Channel)
  object ChannelCreate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.ChannelCreate) Some(ChannelCreate(Json.extract[Channel](ge.payload().d.jv))) else None }
  case class ChannelUpdate(channel: Channel)
  object ChannelUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.ChannelUpdate) Some(ChannelUpdate(Json.extract[Channel](ge.payload().d.jv))) else None }
  case class ChannelDelete(channel: Channel)
  object ChannelDelete { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.ChannelDelete) Some(ChannelDelete(Json.extract[Channel](ge.payload().d.jv))) else None }

  case class Guild(
    id: String,
    name: String,
    icon: String,
    splash: String,
    ownerId: String,
    region: String,
    afkChannelId: String,
    afkTimeout: Int,
    embedEnabled: Option[Boolean],
    embedChannelId: Option[String],
    verificationLevel: Int,
    defaultMessageNotifications: Int,
    roles: Seq[Role],
    emojis: Seq[Emoji],
    features: Seq[String],
    mfaLevel: Int,
    joinedAt: Option[Instant],
    large: Option[Boolean],
    unavailable: Boolean,
    memberCount: Int,
    members: Seq[GuildMember],
    voiceStates: Seq[VoiceState],
    channels: Seq[Channel],
    presences: Seq[GuildPresence]
  ) extends GuildDef

  case class GuildCreate(guild: Guild)
  object GuildCreate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildCreate) Some(GuildCreate(Json.extract[GatewayEvents.Guild](ge.payload().d.jv))) else None }
  case class GuildUpdate(guild: Guild)
  object GuildUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildUpdate) Some(GuildUpdate(Json.extract[GatewayEvents.Guild](ge.payload().d.jv))) else None }
  case class GuildDelete(guild: UnavailableGuild)
  object GuildDelete { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildDelete) Some(GuildDelete(Json.extract[UnavailableGuild](ge.payload().d.jv))) else None }
  case class GuildBanAdd(guildId: String, user: User)
  object GuildBanAdd {
    def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildBanAdd) {
      val event = ge.payload()
      Some(GuildBanAdd(event.d.guildId.extract, Json.extract[User](event.d.jv)))
    } else None
  }
  case class GuildBanRemove(guildId: String, user: User)
  object GuildBanRemove {
    def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildBanRemove) {
      val event = ge.payload()
      Some(GuildBanRemove(event.d.guildId.extract, Json.extract[User](event.d.jv)))
    } else None
  }
  case class GuildEmojisUpdate(guildId: String, emojis: Seq[Emoji])
  object GuildEmojisUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildEmojisUpdate) Some(Json.extract[GuildEmojisUpdate](ge.payload().d.jv)) else None }
  case class GuildIntegrationUpdate(guildId: String)
  object GuildIntegrationUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildIntegrationUpdate) Some(Json.extract[GuildIntegrationUpdate](ge.payload().d.jv)) else None }
  case class GuildMemberAdd(guildId: String, user: User)
  object GuildMemberAdd {
    def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildMemberAdd) {
      val event = ge.payload()
      Some(GuildMemberAdd(event.d.guildId.extract, Json.extract[User](event.d.jv)))
    } else None
  }
  case class GuildMemberRemove(guildId: String, user: User)
  object GuildMemberRemove { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildMemberRemove) Some(Json.extract[GuildMemberRemove](ge.payload().d.jv)) else None }
  case class GuildMemberUpdate(guildId: String, user: User, roles: Seq[Role])
  object GuildMemberUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildMemberUpdate) Some(Json.extract[GuildMemberUpdate](ge.payload().d.jv)) else None }
  case class GuildMemberChunk(guildId: String, members: Seq[GuildMember])
  object GuildMemberChunk { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildMemberChunk) Some(Json.extract[GuildMemberChunk](ge.payload().d.jv)) else None }
  case class GuildRoleCreate(guildId: String, role: Role)
  object GuildRoleCreate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildRoleCreate) Some(Json.extract[GuildRoleCreate](ge.payload().d.jv)) else None }
  case class GuildRoleUpdate(guildId: String, role: Role)
  object GuildRoleUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildRoleUpdate) Some(Json.extract[GuildRoleUpdate](ge.payload().d.jv)) else None }
  case class GuildRoleDelete(guildId: String, roleId: String)
  object GuildRoleDelete { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.GuildRoleDelete) Some(Json.extract[GuildRoleDelete](ge.payload().d.jv)) else None }

  case class MessageCreate(message: Message)
  object MessageCreate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.MessageCreate) Some(MessageCreate(Json.extract[Message](ge.payload().d.jv))) else None }
  case class MessageUpdate(message: jukebox.discord.MessageUpdate)
  object MessageUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.MessageUpdate) Some(GatewayEvents.MessageUpdate(Json.extract[jukebox.discord.MessageUpdate](ge.payload().d.jv))) else None }
  case class MessageDelete(channelId: String, id: String)
  object MessageDelete { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.MessageDelete) Some(Json.extract[MessageDelete](ge.payload().d.jv)) else None }
  case class MessageDeleteBulk(channelId: String, ids: Seq[String])
  object MessageDeleteBulk { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.MessageDeleteBulk) Some(Json.extract[MessageDeleteBulk](ge.payload().d.jv)) else None }

  case class PresenceUpdate(
    guildId: String,
    user: PresenceUser,
    nick: String,
    roles: Seq[String],
    game: Option[Map[String, String]],
    status: String
  )
  object PresenceUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.PresenceUpdate) Some(Json.extract[PresenceUpdate](ge.payload().d.jv)) else None }
  case class PresenceUser(
    id: String,
    userName: Option[String],
    discriminator: Option[String],
    avatar: Option[String],
    bot: Option[Boolean],
    mfaEnabled: Option[Boolean],
    verified: Option[Boolean],
    email: Option[String]
  )

  case class TypingStart(channelId: String, userId: String, timestamp: Long)
  object TypingStart { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.TypingStart) Some(Json.extract[TypingStart](ge.payload().d.jv)) else None }

  case class UserUpdate(user: User)
  object UserUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.UserUpdate) Some(UserUpdate(Json.extract[User](ge.payload().d.jv))) else None }
  case class VoiceStateUpdate(userId: String, sessionId: String, voiceState: VoiceState)
  object VoiceStateUpdate {
    def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.VoiceStateUpdate) {
      val event = ge.payload()
      Some(VoiceStateUpdate(event.d.user_id.extract, event.d.session_id.extract, Json.extract[VoiceState](event.d.jv)))
    } else None
  }
  case class VoiceServerUpdate(guildId: String, token: String, endpoint: String)
  object VoiceServerUpdate { def unapply(ge: GatewayEvent) = if (ge.tpe == EventType.VoiceServerUpdate) Some(Json.extract[VoiceServerUpdate](ge.payload().d.jv)) else None }

}
