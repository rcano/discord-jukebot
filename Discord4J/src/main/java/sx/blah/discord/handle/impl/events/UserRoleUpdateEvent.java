package sx.blah.discord.handle.impl.events;

import sx.blah.discord.api.events.Event;
import sx.blah.discord.handle.obj.IGuild;
import sx.blah.discord.handle.obj.IRole;
import sx.blah.discord.handle.obj.IUser;

import java.util.List;

/**
 * This event is dispatched when a guild updates a user's roles.
 */
public class UserRoleUpdateEvent extends Event {

	private final List<IRole> oldRoles, newRoles;
	private final IUser user;
	private final IGuild guild;

	public UserRoleUpdateEvent(List<IRole> oldRoles, List<IRole> newRoles, IUser user, IGuild guild) {
		this.oldRoles = oldRoles;
		this.newRoles = newRoles;
		this.user = user;
		this.guild = guild;
	}

	/**
	 * Gets the old roles for the user.
	 *
	 * @return The old roles.
	 */
	public List<IRole> getOldRoles() {
		return oldRoles;
	}

	/**
	 * Gets the new roles for the user.
	 *
	 * @return The new roles.
	 */
	public List<IRole> getNewRoles() {
		return newRoles;
	}

	/**
	 * Gets the user involved.
	 *
	 * @return The user.
	 */
	public IUser getUser() {
		return user;
	}

	/**
	 * Gets the guild involved.
	 *
	 * @return The guild.
	 */
	public IGuild getGuild() {
		return guild;
	}
}
