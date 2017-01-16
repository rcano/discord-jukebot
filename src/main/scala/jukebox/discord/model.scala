package jukebox
package discord

import java.time.Instant

case class User(
  id: String,
  userName: String,
  discriminator: String,
  avatar: String,
  bot: Boolean,
  mfaEnabled: Option[Boolean],
  verified: Option[Boolean],
  email: Option[String]
)

trait GuildDef {
  def id: String
  def name: String
  def icon: String
  def splash: String
  def ownerId: String
  def region: String
  def afkChannelId: String
  def afkTimeout: Int
  def embedEnabled: Option[Boolean]
  def embedChannelId: Option[String]
  def verificationLevel: Int
  def defaultMessageNotifications: Int
  def roles: Seq[Role]
  def emojis: Seq[Emoji]
  def features: Seq[String]
  def mfaLevel: Int
}
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
  mfaLevel: Int
) extends GuildDef

case class UnavailableGuild(id: String, unavailable: Boolean)

case class GuildMember(
  user: User,
  nick: Option[String],
  roles: Seq[String],
  joinedAt: Instant,
  deaf: Boolean,
  mute: Boolean
)

case class GuildPresence(
  user: PresenceUser,
  game: Option[Map[String, String]],
  nick: Option[String],
  status: String
)
case class PresenceUser(val id: String)

case class Role(
  id: String,
  name: String,
  color: Int,
  hoist: Boolean,
  position: Int,
  permissions: Int,
  managed: Boolean,
  mentionable: Boolean
)

case class Emoji(
  id: String,
  name: String,
  roles: Seq[Role],
  requireColons: Boolean,
  managed: Boolean
)

case class VoiceState(
  guildId: Option[String],
  channelId: Option[String],
  userId: String,
  deaf: Boolean,
  mute: Boolean,
  selfDeaf: Boolean,
  selfMute: Boolean,
  suppress: Boolean
)

sealed trait Channel {
  def id: String
  //  def guildId: String
  def name: String
  def position: Int
  def isPrivate: Boolean
  def permissionOverwrites: Seq[Overwrite]
}

case class TextChannel(
  id: String,
  //  guildId: String,
  name: String,
  position: Int,
  isPrivate: Boolean,
  permissionOverwrites: Seq[Overwrite],
  topic: String,
  lastMessageId: String
) extends Channel

case class VoiceChannel(
  id: String,
  //  guildId: String,
  name: String,
  position: Int,
  isPrivate: Boolean,
  permissionOverwrites: Seq[Overwrite],
  bitrate: Int,
  userLimit: Int
) extends Channel

case class Overwrite(id: String, tpe: String, allow: Int, deny: Int)

case class DmChannel(
  id: String,
  isPrivate: Boolean,
  recipient: User,
  lastMessageId: String
)

case class Message(
  id: String,
  channelId: String,
  author: User,
  content: String,
  timestamp: Instant,
  editedTimestamp: Option[Instant],
  tts: Boolean,
  mentionEveryone: Boolean,
  mentions: Seq[User],
  mentionRoles: Seq[String],
  attachments: Seq[Attachment],
  embeds: Seq[Embed],
  nonce: Option[String],
  pinned: Boolean,
  webhookId: Option[String]
)
case class MessageUpdate(
  id: String,
  channelId: String,
  content: Option[String],
  editedTimestamp: Option[Instant],
  tts: Option[Boolean],
  mentionEveryone: Option[Boolean],
  mentions: Seq[User],
  mentionRoles: Seq[String],
  attachments: Seq[Attachment],
  embeds: Seq[Embed],
  nonce: Option[String],
  pinned: Option[Boolean],
  webhookId: Option[String]
)

case class Embed(
  tpe: String,
  title: Option[String] = None,
  description: Option[String] = None,
  url: Option[String] = None,
  timestamp: Option[Instant] = None,
  color: Option[Int] = None,
  footer: Option[EmbedFooter] = None,
  image: Option[EmbedImage] = None,
  thumbnail: Option[EmbedThumbnail] = None,
  video: Option[EmbedVideo] = None,
  provider: Option[EmbedProvider] = None,
  author: Option[EmbedAuthor] = None,
  fields: Seq[EmbedField] = Seq.empty
)
case class EmbedThumbnail(
  url: String,
  proxyUrl: String,
  height: Int,
  width: Int
)
case class EmbedVideo(
  url: String,
  height: Int,
  width: Int
)
case class EmbedImage(
  url: String,
  proxyUrl: String,
  height: Int,
  width: Int
)
case class EmbedProvider(name: String, url: String)
case class EmbedAuthor(
  name: String,
  url: Option[String] = None,
  iconUrl: Option[String] = None,
  proxyIconUrl: Option[String] = None
)
case class EmbedFooter(
  text: String,
  iconUrl: Option[String] = None,
  proxyIconUrl: Option[String] = None
)
case class EmbedField(
  name: String,
  value: String,
  inline: Boolean
)

case class Attachment(
  id: String,
  filename: String,
  size: Int,
  url: String,
  proxyUrl: String,
  height: Option[Int] = None,
  width: Option[Int] = None
)

sealed trait Status
object Status {
  case class PlayingGame(game: String) extends Status
  case class Streaming(name: String, url: String) extends Status
  case object Empty extends Status
}