package jukebox.discord

import java.time.Instant

object GatewayEvents {

  sealed trait GatewayEvent
  case class Ready(
    v: Int,
    user: User,
    privateChannels: Seq[DmChannel],
    guilds: Seq[UnavailableGuild],
    /* presences: Seq[Any],
     relationships: Seq[Any], */
    _trace: Seq[String]
  ) extends GatewayEvent

  case object Resumed extends GatewayEvent

  case class ChannelCreate(channel: Channel) extends GatewayEvent
  case class ChannelUpdate(channel: Channel) extends GatewayEvent
  case class ChannelDelete(channel: Channel) extends GatewayEvent

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
  ) extends GuildDef with GatewayEvent

  case class GuildCreate(guild: Guild) extends GatewayEvent
  case class GuildUpdate(guild: Guild) extends GatewayEvent
  case class GuildDelete(guild: UnavailableGuild) extends GatewayEvent
  case class GuildBanAdd(guildId: String, user: User) extends GatewayEvent
  case class GuildBanRemove(guildId: String, user: User) extends GatewayEvent
  case class GuildEmojisUpdate(guildId: String, emojis: Seq[Emoji]) extends GatewayEvent
  case class GuildIntegrationUpdate(guildId: String) extends GatewayEvent
  case class GuildMemberAdd(guildId: String, user: User) extends GatewayEvent
  case class GuildMemberRemove(guildId: String, user: User) extends GatewayEvent
  case class GuildMemberUpdate(guildId: String, user: User, roles: Seq[Role]) extends GatewayEvent
  case class GuildMemberChunk(guildId: String, members: Seq[GuildMember]) extends GatewayEvent
  case class GuildRoleCreate(guildId: String, role: Role) extends GatewayEvent
  case class GuildRoleUpdate(guildId: String, role: Role) extends GatewayEvent
  case class GuildRoleDelete(guildId: String, roleId: String) extends GatewayEvent

  case class MessageCreate(message: Message) extends GatewayEvent
  case class MessageUpdate(message: jukebox.discord.MessageUpdate) extends GatewayEvent
  case class MessageDelete(channelId: String, id: String) extends GatewayEvent
  case class MessageDeleteBulk(channelId: String, ids: Seq[String]) extends GatewayEvent

  case class PresenceUpdate(
    guildId: String,
    user: PresenceUser,
    nick: String,
    roles: Seq[String],
    game: Option[Map[String, String]],
    status: String
  ) extends GatewayEvent
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

  case class TypingStart(channelId: String, userId: String, timestamp: Long) extends GatewayEvent

  case class UserUpdate(user: User) extends GatewayEvent
  case class VoiceStateUpdate(userId: String, sessionId: String, voiceState: VoiceState) extends GatewayEvent
  case class VoiceServerUpdate(guildId: String, token: String, endpoint: String) extends GatewayEvent

}
